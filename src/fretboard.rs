//! Fretboard diagram rendering, ported from XenFret.hs.
//!
//! Coordinate conventions
//! ─────────────────────
//! • The neck rect spans [0, board_w] × [0, h] where board_w = w + 2·pad.
//! • Strings are inset by `pad = hs * NECK_PAD` from each neck edge.
//! • String i is therefore at x = pad + i·hs.
//! • Fret i is at y = i * vs (y=0 is the nut).
//! • All sizing constants are expressed as multiples of `vs` (or `hs`) so the
//!   diagram scales uniformly when the user changes spacing.

use hagoromo::{
    circle, hrule, polygon, polyline, rect, render_svg, stroke_trail, style::Color, text, vrule,
    Diagram, Point, RenderOptions, WHITE,
};

use crate::models::{FretMarker, FretStyle, Preferences, Scale, Tuning};

// ── Design tokens (relative to `vs`) ─────────────────────────────────────────

const GRID_COLOR: Color = Color::rgb(0.20, 0.20, 0.20);

const NUT_W: f64 = 0.12; // nut line width / vs
const LINE_W: f64 = 0.055; // fret line width / vs
const STRING_W: f64 = 0.018; // string line width / vs
const NECK_BORDER_W: f64 = 0.055; // neck outline width / vs
const DOT_R: f64 = 0.21; // note dot radius / vs
const NECK_PAD: f64 = 0.45; // side inset between outer string and neck edge / hs

// ── Public style struct ───────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct FretboardStyle {
    pub display_markers_on_frets: bool,
    pub fret_offset: u32,
    pub num_frets: u32,
    pub vertical_spacing: f64,
    pub horizontal_spacing: f64,
    pub horizontal: bool,
    pub edo: u32,
    /// Period ratio (e.g. 2.0 for octave, 3.0 for tritave/Bohlen-Pierce).
    pub period: f64,
    /// Mirror the diagram for left-handed players (nut on right for horizontal,
    /// lowest string on right for vertical).
    pub left_handed: bool,
}

impl Default for FretboardStyle {
    fn default() -> Self {
        Self {
            display_markers_on_frets: false,
            fret_offset: 0,
            num_frets: 12,
            vertical_spacing: 0.2,
            horizontal_spacing: 0.2,
            horizontal: false,
            edo: 12,
            period: 2.0,
            left_handed: false,
        }
    }
}

// ── Internal note type ────────────────────────────────────────────────────────

struct Note {
    scale_degree: usize,
    pitch: i32,
    /// Absolute EDO step of this note (= raw_pitch + key from get_notes).
    /// Used for octave-aware "is this note currently playing?" checks.
    absolute_step: i32,
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn model_color(c: &crate::models::Color) -> Color {
    Color::rgb(c.r as f32 / 255.0, c.g as f32 / 255.0, c.b as f32 / 255.0)
}

fn display_note(pitch: i32, note_names: &[String]) -> String {
    if note_names.is_empty() {
        return pitch.to_string();
    }
    let idx = pitch.rem_euclid(note_names.len() as i32) as usize;
    note_names[idx].clone()
}

/// Returns the font-family for a label. Only SMuFL/PUA characters (U+E000+)
/// need Bravura; everything else uses plain sans-serif. Applying Bravura to
/// ASCII-only labels would inflate their line-height to 1.46em (Bravura's
/// usWinAscent + usWinDescent) even though the glyph comes from the fallback.
fn note_font(name: &str) -> &'static str {
    if name.chars().any(|c| c as u32 >= 0xE000) {
        "Bravura, sans-serif"
    } else {
        "sans-serif"
    }
}

/// Normalized position of fret `k` along a neck of `n` frets, where the
/// tuning has `edo` equal divisions of `period` (e.g. period=2 for octave,
/// period=3 for Bohlen-Pierce tritave). Returns 0.0 at nut, 1.0 at last fret.
fn fret_pos(k: f64, n: f64, edo: f64, period: f64) -> f64 {
    if n <= 0.0 || edo <= 0.0 || period <= 1.0 {
        return k / n.max(1.0);
    }
    let denom = 1.0 - period.powf(-n / edo);
    if denom.abs() < 1e-12 {
        return k / n;
    }
    (1.0 - period.powf(-k / edo)) / denom
}

// ── repeatingNotes ────────────────────────────────────────────────────────────

fn repeating_notes(scale: &Scale) -> impl Iterator<Item = (usize, i32)> + '_ {
    let period_size: i32 = scale.intervals.iter().sum();
    let len = scale.intervals.len();
    let mut current = -period_size;
    let mut j = 0usize;
    std::iter::from_fn(move || {
        if len == 0 {
            return None;
        }
        let degree = j % len;
        let item = (degree, current);
        current += scale.intervals[j % len];
        j += 1;
        Some(item)
    })
}

// ── getNotes ──────────────────────────────────────────────────────────────────

fn get_notes(scale: &Scale, tuning: &Tuning, key: i32, skip_frets: u32) -> Vec<Vec<Note>> {
    let n = (skip_frets + 1) as i32;
    let period_size: i32 = scale.intervals.iter().sum();
    if period_size <= 0 || scale.intervals.is_empty() {
        return tuning.string_tunings.iter().map(|_| vec![]).collect();
    }
    const MAX_FRET: i32 = 36;
    tuning
        .string_tunings
        .iter()
        .map(|&string_pitch| {
            let mut notes = Vec::new();
            for (degree, raw_pitch) in repeating_notes(scale) {
                let numerator = raw_pitch + key - string_pitch;
                if numerator % n == 0 {
                    let fret = numerator / n;
                    if fret >= 0 {
                        if fret > MAX_FRET {
                            break;
                        }
                        notes.push(Note {
                            scale_degree: degree,
                            pitch: fret,
                            absolute_step: raw_pitch + key,
                        });
                    }
                }
                if raw_pitch - string_pitch + key > (MAX_FRET + 2) * n + period_size {
                    break;
                }
            }
            notes
        })
        .collect()
}

// ── frettingDot ───────────────────────────────────────────────────────────────

const PLAYING_COLOR: Color = Color::rgb(0.90, 0.18, 0.18);

fn note_color(
    scale_degree: usize,
    absolute_step: i32,
    root_color: Color,
    scale_color: Color,
    playing_degrees: &[usize],
    playing_steps: &[i32],
) -> Color {
    if playing_steps.contains(&absolute_step) || playing_degrees.contains(&scale_degree) {
        PLAYING_COLOR
    } else if scale_degree == 0 {
        root_color
    } else {
        scale_color
    }
}

fn fretting_dot(
    root_color: Color,
    scale_color: Color,
    playing_degrees: &[usize],
    playing_steps: &[i32],
    display_markers_on_frets: bool,
    original_offset: i32,
    vs: f64,
    note: &Note,
    string_x: f64,
) -> Diagram {
    let n = note.pitch as f64;
    let color = note_color(
        note.scale_degree,
        note.absolute_step,
        root_color,
        scale_color,
        playing_degrees,
        playing_steps,
    );

    // Open-string note: filled dot above the nut, same style as fretted notes.
    if original_offset == 0 && note.pitch == 0 {
        let radius = vs * DOT_R;
        let dot_y = -vs * 0.5; // centered above the nut, below the string-name labels
        let dot = circle(radius).fc(color).lw(0.0).translate(string_x, dot_y);
        let label_size = radius * 1.1;
        let label = text(format!("{}", note.scale_degree + 1), label_size)
            .fc(WHITE)
            .bold()
            .font_family("sans-serif")
            .translate(string_x, dot_y + label_size * 0.06);
        return dot + label;
    }

    let offset_y = if display_markers_on_frets {
        0.0
    } else {
        0.5 * vs
    };
    let dot_y = n * vs - offset_y;
    let radius = vs * DOT_R;

    let dot = circle(radius).fc(color).lw(0.0).translate(string_x, dot_y);
    let label_size = radius * 1.1;
    let label = text(format!("{}", note.scale_degree + 1), label_size)
        .fc(WHITE)
        .bold()
        .font_family("sans-serif")
        .translate(string_x, dot_y + label_size * 0.06);

    dot + label
}

// ── emptyBoard ────────────────────────────────────────────────────────────────

fn empty_board(
    board_color: Color,
    _fret_style: FretStyle,
    n_frets: usize,
    vs: f64,
    hs: f64,
    n_str: usize,
    offset: usize,
) -> Diagram {
    let h = n_frets as f64 * vs;
    let w = (n_str as f64 - 1.0).max(0.0) * hs;
    let pad = hs * NECK_PAD;
    let board_w = w + 2.0 * pad;

    let nut_lw = vs * NUT_W;
    let fret_lw = vs * LINE_W;
    let string_lw = vs * STRING_W;
    let border_lw = vs * NECK_BORDER_W;

    let bg = rect(board_w, h)
        .fc(board_color)
        .lc(GRID_COLOR)
        .stroke_width(border_lw)
        .translate(board_w / 2.0, h / 2.0);

    let nut = stroke_trail(hrule(board_w))
        .lc(GRID_COLOR)
        .lw(if offset == 0 { nut_lw } else { fret_lw });

    let fret_lines: Diagram = (1..=n_frets)
        .map(|i| {
            stroke_trail(hrule(board_w))
                .lc(GRID_COLOR)
                .lw(fret_lw)
                .translate_y(i as f64 * vs)
        })
        .fold(Diagram::empty(), |acc, d| acc + d);

    let strings: Diagram = (0..n_str)
        .map(|i| {
            stroke_trail(vrule(h))
                .lc(GRID_COLOR)
                .lw(string_lw)
                .translate_x(pad + i as f64 * hs)
        })
        .fold(Diagram::empty(), |acc, d| acc + d);

    bg + nut + fret_lines + strings
}

// ── render_board ──────────────────────────────────────────────────────────────

pub fn render_board(
    prefs: &Preferences,
    scale_name: &str,
    key: i32,
    scale: &Scale,
    skip_frets: u32,
    tuning: &Tuning,
    style: &FretboardStyle,
    note_names: Option<&[String]>,
    font_url: &str,
    playing_degrees: &[usize],
    playing_steps: &[i32],
    dark: bool,
) -> String {
    let names = note_names.unwrap_or(&[]);
    let diagram = if style.horizontal {
        board_horizontal(
            prefs,
            scale_name,
            key,
            scale,
            skip_frets,
            tuning,
            style,
            names,
            playing_degrees,
            playing_steps,
            dark,
        )
    } else {
        board_vertical(
            prefs,
            scale_name,
            key,
            scale,
            skip_frets,
            tuning,
            style,
            names,
            playing_degrees,
            playing_steps,
            dark,
        )
    };
    let svg = render_svg(
        &diagram,
        &RenderOptions {
            padding: 0.05,
            background: None,
            default_stroke_width: hagoromo::style::THIN,
        },
    );
    // Inject @font-face directly into the SVG so the Bravura font loads
    // correctly when the SVG is embedded as inline HTML (dangerous_inner_html).
    // The CSS @font-face in style.css alone is not reliably picked up for
    // inline SVG by Chrome's font sanitizer.
    // WebKit (Safari / Dioxus desktop WebView) collapses inline SVGs that have
    // only a viewBox but no explicit width.  Chrome infers the size from the
    // container; WebKit does not.  Adding width="100%" fixes both.
    let svg = svg.replacen("<svg ", "<svg width=\"100%\" ", 1);
    let needs_font = names.iter().any(|n| note_font(n) != "sans-serif");
    if needs_font {
        inject_bravura_font(svg, font_url)
    } else {
        svg
    }
}

fn inject_bravura_font(svg: String, font_url: &str) -> String {
    let fmt = if font_url.ends_with(".woff2") {
        "woff2"
    } else {
        "woff"
    };
    let defs = format!(
        "\n  <defs><style>@font-face{{font-family:'Bravura';src:url('{}') format('{}');font-display:swap}}</style></defs>",
        font_url, fmt
    );
    match svg.find('>') {
        Some(pos) => [&svg[..=pos], &defs, &svg[pos + 1..]].concat(),
        None => svg,
    }
}

// ── board_vertical ────────────────────────────────────────────────────────────

fn board_vertical(
    prefs: &Preferences,
    scale_name: &str,
    key: i32,
    scale: &Scale,
    skip_frets: u32,
    tuning: &Tuning,
    style: &FretboardStyle,
    note_names: &[String],
    playing_degrees: &[usize],
    playing_steps: &[i32],
    dark: bool,
) -> Diagram {
    let vs = style.vertical_spacing;
    let hs = style.horizontal_spacing;
    let n_frets = style.num_frets as usize;
    let offset = style.fret_offset as i32;
    let n_str = tuning.string_tunings.len();

    if n_str == 0 || n_frets == 0 {
        return Diagram::empty();
    }

    let (rc, sc, bc, lc) = prefs.active_diagram_colors(dark);
    let root_color = model_color(rc);
    let scale_color = model_color(sc);
    let board_color = model_color(bc);
    let label_color = model_color(lc);
    let w = (n_str as f64 - 1.0) * hs;
    let pad = hs * NECK_PAD;
    let board_w = w + 2.0 * pad;
    let has_names = !note_names.is_empty();
    let note_fs = vs * 0.36;

    // ── Notes ─────────────────────────────────────────────────────────────────
    let positions: Vec<Vec<Note>> = get_notes(scale, tuning, key, skip_frets)
        .into_iter()
        .map(|sn| {
            sn.into_iter()
                .take_while(|n| n.pitch <= n_frets as i32 + offset)
                .skip_while(|n| n.pitch < offset)
                .map(|n| Note {
                    scale_degree: n.scale_degree,
                    pitch: n.pitch - offset,
                    absolute_step: n.absolute_step,
                })
                // When offset > 0 the nut isn't shown, so drop notes exactly at
                // the boundary (adjusted pitch 0) — they'd render above the board.
                .filter(|n| !(offset > 0 && n.pitch == 0))
                .collect()
        })
        .collect();

    // ── Dots ──────────────────────────────────────────────────────────────────
    let has_open_dots = offset == 0
        && positions
            .iter()
            .any(|notes| notes.iter().any(|n| n.pitch == 0));

    let dots: Diagram = positions
        .iter()
        .enumerate()
        .map(|(i, notes)| {
            // Left-handed: mirror string order so lowest string is on the right.
            let j = if style.left_handed { n_str - 1 - i } else { i };
            let sx = pad + j as f64 * hs;
            notes.iter().fold(Diagram::empty(), |acc, note| {
                acc + fretting_dot(
                    root_color,
                    scale_color,
                    playing_degrees,
                    playing_steps,
                    style.display_markers_on_frets,
                    offset,
                    vs,
                    note,
                    sx,
                )
            })
        })
        .fold(Diagram::empty(), |acc, d| acc + d);

    let neck_markers = fret_marker_dots_v(
        &tuning.fret_markers,
        vs,
        hs,
        n_frets,
        style.fret_offset,
        n_str,
        style.display_markers_on_frets,
    );
    let fretboard_body = empty_board(
        board_color,
        prefs.fret_style,
        n_frets,
        vs,
        hs,
        n_str,
        offset as usize,
    ) + neck_markers
        + dots;

    // ── String markers (above nut) ────────────────────────────────────────────
    // When open-string dots are present they sit at -0.5·vs; push string names
    // high enough to clear them.  Without open dots, sit closer to the nut.
    let string_name_y = if has_open_dots { -vs * 1.0 } else { -vs * 0.65 };
    let string_markers: Diagram = if has_names {
        tuning
            .string_tunings
            .iter()
            .enumerate()
            .fold(Diagram::empty(), |acc, (i, &pitch)| {
                let name = display_note(pitch, note_names);
                let j = if style.left_handed { n_str - 1 - i } else { i };
                acc + text(name.clone(), note_fs)
                    .fc(label_color)
                    .bold()
                    .font_family(note_font(&name))
                    .translate(pad + j as f64 * hs, string_name_y)
            })
    } else {
        Diagram::empty()
    };

    // ── Fret markers (left or right column depending on handedness) ───────────
    let fret_markers: Diagram = if has_names {
        let lowest = tuning.string_tunings.first().copied().unwrap_or(0);
        let leading_y = if offset == 0 {
            if style.display_markers_on_frets {
                vs
            } else {
                vs * 0.5
            }
        } else if style.display_markers_on_frets {
            vs * 0.5
        } else {
            0.0
        };
        // Left-handed: labels go on the right side of the neck.
        let label_x = if style.left_handed { board_w + hs * 0.55 } else { -hs * 0.55 };

        (0..n_frets).fold(Diagram::empty(), |acc, i| {
            let name = display_note(offset + lowest + i as i32 + 1, note_names);
            let y = leading_y + i as f64 * vs;
            acc + text(name.clone(), note_fs)
                .fc(label_color)
                .bold()
                .font_family(note_font(&name))
                .translate(label_x, y)
        })
    } else {
        Diagram::empty()
    };

    // ── Title ─────────────────────────────────────────────────────────────────
    let title_text = if has_names {
        format!("{} {}", display_note(key, note_names), scale_name)
    } else {
        scale_name.to_string()
    };
    let title_font = note_font(&title_text);
    let title_y = if has_names {
        if has_open_dots {
            -vs * 1.6
        } else {
            -vs * 1.25
        }
    } else {
        -vs * 0.65
    };
    let title = text(title_text, vs * 0.52)
        .fc(label_color)
        .bold()
        .font_family(title_font)
        .translate(board_w / 2.0, title_y);

    title + string_markers + fret_markers + fretboard_body
}

// ── empty_board_h ─────────────────────────────────────────────────────────────
// Horizontal orientation: nut on left, fret lines vertical, strings horizontal.

fn empty_board_h(
    board_color: Color,
    _fret_style: FretStyle,
    n_frets: usize,
    edo: u32,
    period: f64,
    vs: f64,
    hs: f64,
    n_str: usize,
    offset: usize,
    board_w: f64,
    left_handed: bool,
) -> Diagram {
    let h_str = (n_str as f64 - 1.0).max(0.0) * hs;
    let pad = hs * NECK_PAD;
    let board_h = h_str + 2.0 * pad;
    // Neck widens by `taper` on each side from nut to body end.
    let taper = h_str * 0.04;
    let edo_f = edo as f64;
    let n_f = n_frets as f64;

    // Line widths: mostly based on hs (EDO-independent) with a small vs
    // contribution so frets thin very slightly at higher EDOs.
    let lw_base = hs * 0.75 + vs * 0.25;
    let nut_lw = lw_base * NUT_W;
    let fret_lw = lw_base * LINE_W;
    let string_lw = lw_base * STRING_W;
    let border_lw = lw_base * NECK_BORDER_W;

    // Left-handed: mirror everything around x = board_w/2.
    let fx = |x: f64| if left_handed { board_w - x } else { x };
    // Nut is at x=0 (right-handed) or x=board_w (left-handed).
    let nut_x = fx(0.0);
    let body_x = fx(board_w);

    // Trapezoid background: nut end is narrow, body end flares by `taper`.
    let verts = [
        Point::new(nut_x, 0.0),
        Point::new(body_x, -taper),
        Point::new(body_x, board_h + taper),
        Point::new(nut_x, board_h),
    ];
    let cx = board_w / 2.0;
    let cy = (0.0 + (-taper) + (board_h + taper) + board_h) / 4.0;
    let bg = polygon(&verts)
        .fc(board_color)
        .lc(GRID_COLOR)
        .stroke_width(border_lw)
        .translate(cx, cy);

    // Fret lines at logarithmically-spaced x positions, tapering in height.
    let fret_lines: Diagram = (0..=n_frets)
        .map(|k| {
            let x = fx(fret_pos(k as f64, n_f, edo_f, period) * board_w);
            let taper_frac = if left_handed { 1.0 - x / board_w } else { x / board_w };
            let y_top = -(taper * taper_frac);
            let y_bot = board_h + taper * taper_frac;
            let is_nut = k == 0 && offset == 0;
            let lw = if is_nut { nut_lw } else { fret_lw };
            polyline(&[Point::new(x, y_top), Point::new(x, y_bot)])
                .lc(GRID_COLOR)
                .lw(lw)
        })
        .fold(Diagram::empty(), |acc, d| acc + d);

    // Strings: angled lines following the neck taper.
    // String 0 (lowest pitch) is at the BOTTOM so the diagram matches a player's
    // eye view: low strings near them, high strings away.
    let strings: Diagram = (0..n_str)
        .map(|i| {
            let j = (n_str - 1 - i) as f64; // flip: i=0 → bottom
            let y_nut = pad + j * hs;
            let y_body = if n_str <= 1 {
                y_nut
            } else {
                pad - taper + j * (h_str + 2.0 * taper) / (n_str as f64 - 1.0)
            };
            polyline(&[Point::new(nut_x, y_nut), Point::new(body_x, y_body)])
                .lc(GRID_COLOR)
                .lw(string_lw)
        })
        .fold(Diagram::empty(), |acc, d| acc + d);

    bg + fret_lines + strings
}

// ── fretting_dot_h ────────────────────────────────────────────────────────────

fn fretting_dot_h(
    root_color: Color,
    scale_color: Color,
    playing_degrees: &[usize],
    playing_steps: &[i32],
    display_markers_on_frets: bool,
    original_offset: i32,
    vs: f64,
    hs: f64,
    note: &Note,
    // y position of this string at the nut and body end (x=0 / x=board_w
    // before any left-handed flip), so the dot sits on the angled string.
    y_nut: f64,
    y_body: f64,
    n_frets: usize,
    edo: u32,
    period: f64,
    board_w: f64,
    left_handed: bool,
) -> Diagram {
    let color = note_color(
        note.scale_degree,
        note.absolute_step,
        root_color,
        scale_color,
        playing_degrees,
        playing_steps,
    );
    // Size dots by hs (string spacing) so they stay consistent as EDO changes.
    let radius = hs * DOT_R * 0.82;

    let fx = |x: f64| if left_handed { board_w - x } else { x };

    // Interpolate string y at a given pre-flip x.
    let string_y_at = |x_raw: f64| y_nut + (y_body - y_nut) * (x_raw / board_w);

    // Open-string note: dot outside the nut (left for right-handed, right for left-handed).
    if original_offset == 0 && note.pitch == 0 {
        let dot_x = fx(if left_handed { board_w + vs * 0.5 } else { -vs * 0.5 });
        let sy = y_nut;
        let dot = circle(radius).fc(color).lw(0.0).translate(dot_x, sy);
        let label_size = radius * 1.1;
        let label = text(format!("{}", note.scale_degree + 1), label_size)
            .fc(WHITE)
            .bold()
            .font_family("sans-serif")
            .translate(dot_x, sy + label_size * 0.06);
        return dot + label;
    }

    let k = note.pitch as f64;
    let n = n_frets as f64;
    let edo_f = edo as f64;
    let dot_x_raw = if display_markers_on_frets {
        fret_pos(k, n, edo_f, period) * board_w
    } else {
        let x_prev = fret_pos((k - 1.0).max(0.0), n, edo_f, period) * board_w;
        let x_curr = fret_pos(k, n, edo_f, period) * board_w;
        (x_prev + x_curr) / 2.0
    };
    let dot_x = fx(dot_x_raw);
    let sy = string_y_at(dot_x_raw);

    let sy = string_y_at(dot_x);
    let dot = circle(radius).fc(color).lw(0.0).translate(dot_x, sy);
    let label_size = radius * 1.1;
    let label = text(format!("{}", note.scale_degree + 1), label_size)
        .fc(WHITE)
        .bold()
        .font_family("sans-serif")
        .translate(dot_x, sy + label_size * 0.06);

    dot + label
}

// ── fret_marker_dots ─────────────────────────────────────────────────────────

const MARKER_COLOR: Color = Color::rgb(0.72, 0.72, 0.72);

/// Render neck position markers (inlays) for a vertical board.
/// Markers sit centered horizontally between adjacent fret lines.
fn fret_marker_dots_v(
    markers: &[(u32, FretMarker)],
    vs: f64,
    hs: f64,
    n_frets: usize,
    offset: u32,
    n_str: usize,
    display_markers_on_frets: bool,
) -> Diagram {
    if n_str < 2 {
        return Diagram::empty();
    }
    let pad = hs * NECK_PAD;
    let w = (n_str as f64 - 1.0) * hs;
    let cx = pad + w / 2.0; // horizontal centre of neck
    let r = vs * 0.15;

    markers
        .iter()
        .filter_map(|&(fret, kind)| {
            let adjusted = fret.checked_sub(offset)?;
            if adjusted == 0 || adjusted as usize > n_frets {
                return None;
            }
            let a = adjusted as f64;
            let dot_y = if display_markers_on_frets {
                a * vs
            } else {
                (a - 0.5) * vs
            };
            Some(match kind {
                FretMarker::Single => circle(r).fc(MARKER_COLOR).lw(0.0).translate(cx, dot_y),
                FretMarker::Double => {
                    let offset_x = hs * 0.35;
                    circle(r)
                        .fc(MARKER_COLOR)
                        .lw(0.0)
                        .translate(cx - offset_x, dot_y)
                        + circle(r)
                            .fc(MARKER_COLOR)
                            .lw(0.0)
                            .translate(cx + offset_x, dot_y)
                }
            })
        })
        .fold(Diagram::empty(), |acc, d| acc + d)
}

/// Render neck position markers (inlays) for a horizontal board.
/// Markers sit centered vertically between adjacent fret lines.
fn fret_marker_dots_h(
    markers: &[(u32, FretMarker)],
    vs: f64,
    hs: f64,
    n_frets: usize,
    offset: u32,
    n_str: usize,
    display_markers_on_frets: bool,
    board_w: f64,
    edo: u32,
    period: f64,
    left_handed: bool,
) -> Diagram {
    if n_str < 2 {
        return Diagram::empty();
    }
    let fx = |x: f64| if left_handed { board_w - x } else { x };
    let pad = hs * NECK_PAD;
    let cy = pad + (n_str as f64 - 1.0) * hs / 2.0; // vertical centre of neck
    let r = hs * 0.15;
    let n_f = n_frets as f64;
    let edo_f = edo as f64;

    markers
        .iter()
        .filter_map(|&(fret, kind)| {
            let adjusted = fret.checked_sub(offset)?;
            if adjusted == 0 || adjusted as usize > n_frets {
                return None;
            }
            let a = adjusted as f64;
            let dot_x = fx(if display_markers_on_frets {
                fret_pos(a, n_f, edo_f, period) * board_w
            } else {
                let x0 = fret_pos(a - 1.0, n_f, edo_f, period) * board_w;
                let x1 = fret_pos(a, n_f, edo_f, period) * board_w;
                (x0 + x1) / 2.0
            });
            Some(match kind {
                FretMarker::Single => circle(r).fc(MARKER_COLOR).lw(0.0).translate(dot_x, cy),
                FretMarker::Double => {
                    let off_y = hs * 0.35;
                    circle(r)
                        .fc(MARKER_COLOR)
                        .lw(0.0)
                        .translate(dot_x, cy - off_y)
                        + circle(r)
                            .fc(MARKER_COLOR)
                            .lw(0.0)
                            .translate(dot_x, cy + off_y)
                }
            })
        })
        .fold(Diagram::empty(), |acc, d| acc + d)
}

// ── board_horizontal ──────────────────────────────────────────────────────────

fn board_horizontal(
    prefs: &Preferences,
    scale_name: &str,
    key: i32,
    scale: &Scale,
    skip_frets: u32,
    tuning: &Tuning,
    style: &FretboardStyle,
    note_names: &[String],
    playing_degrees: &[usize],
    playing_steps: &[i32],
    dark: bool,
) -> Diagram {
    // vs = fret pitch (now along the x-axis), hs = string pitch (along y-axis).
    let vs = style.vertical_spacing;
    let hs = style.horizontal_spacing;
    let n_frets = style.num_frets as usize;
    let offset = style.fret_offset as i32;
    let n_str = tuning.string_tunings.len();
    let left_handed = style.left_handed;

    if n_str == 0 || n_frets == 0 {
        return Diagram::empty();
    }

    let (rc, sc, bc, lc) = prefs.active_diagram_colors(dark);
    let root_color = model_color(rc);
    let scale_color = model_color(sc);
    let board_color = model_color(bc);
    let label_color = model_color(lc);
    let pad = hs * NECK_PAD;
    let board_w = n_frets as f64 * vs;
    let h_str = (n_str as f64 - 1.0).max(0.0) * hs;
    let taper = h_str * 0.04;
    let has_names = !note_names.is_empty();
    // vs shrinks with EDO to keep board width constant, so size by hs instead.
    let note_fs = hs * 0.36;

    // ── Notes ─────────────────────────────────────────────────────────────────
    let positions: Vec<Vec<Note>> = get_notes(scale, tuning, key, skip_frets)
        .into_iter()
        .map(|sn| {
            sn.into_iter()
                .take_while(|n| n.pitch <= n_frets as i32 + offset)
                .skip_while(|n| n.pitch < offset)
                .map(|n| Note {
                    scale_degree: n.scale_degree,
                    pitch: n.pitch - offset,
                    absolute_step: n.absolute_step,
                })
                .filter(|n| !(offset > 0 && n.pitch == 0))
                .collect()
        })
        .collect();

    // ── Dots ──────────────────────────────────────────────────────────────────
    let has_open_dots = offset == 0
        && positions
            .iter()
            .any(|notes| notes.iter().any(|n| n.pitch == 0));

    let dots: Diagram = positions
        .iter()
        .enumerate()
        .map(|(i, notes)| {
            let j = (n_str - 1 - i) as f64; // flip: string 0 (lowest) at bottom
            let y_nut = pad + j * hs;
            let y_body = if n_str <= 1 {
                y_nut
            } else {
                pad - taper + j * (h_str + 2.0 * taper) / (n_str as f64 - 1.0)
            };
            notes.iter().fold(Diagram::empty(), |acc, note| {
                acc + fretting_dot_h(
                    root_color,
                    scale_color,
                    playing_degrees,
                    playing_steps,
                    style.display_markers_on_frets,
                    offset,
                    vs,
                    hs,
                    note,
                    y_nut,
                    y_body,
                    n_frets,
                    style.edo,
                    style.period,
                    board_w,
                    left_handed,
                )
            })
        })
        .fold(Diagram::empty(), |acc, d| acc + d);

    let neck_markers = fret_marker_dots_h(
        &tuning.fret_markers,
        vs,
        hs,
        n_frets,
        style.fret_offset,
        n_str,
        style.display_markers_on_frets,
        board_w,
        style.edo,
        style.period,
        left_handed,
    );
    let fretboard_body = empty_board_h(
        board_color,
        prefs.fret_style,
        n_frets,
        style.edo,
        style.period,
        vs,
        hs,
        n_str,
        offset as usize,
        board_w,
        left_handed,
    ) + neck_markers
        + dots;

    // ── String markers (outside the nut) ─────────────────────────────────────
    // Open-string dots sit just outside the nut; push string names further out.
    // For left-handed, "outside" is to the right (x > board_w).
    let string_name_x = if left_handed {
        if has_open_dots { board_w + vs * 1.0 } else { board_w + vs * 0.65 }
    } else {
        if has_open_dots { -vs * 1.0 } else { -vs * 0.65 }
    };
    let string_markers: Diagram = if has_names {
        tuning
            .string_tunings
            .iter()
            .enumerate()
            .fold(Diagram::empty(), |acc, (i, &pitch)| {
                let name = display_note(pitch, note_names);
                let j = (n_str - 1 - i) as f64; // flip: string 0 at bottom
                let sy = pad + j * hs;
                acc + text(name.clone(), note_fs)
                    .fc(label_color)
                    .bold()
                    .font_family(note_font(&name))
                    .translate(string_name_x, sy)
            })
    } else {
        Diagram::empty()
    };

    // ── Fret markers (above neck) ─────────────────────────────────────────────
    let fret_markers: Diagram = if has_names {
        let lowest = tuning.string_tunings.first().copied().unwrap_or(0);
        // Place labels above the highest point of the tapered top edge (-taper)
        // with enough clearance for the text height (note_fs).
        let label_y = -(taper + note_fs * 1.0);
        let edo_f = style.edo as f64;
        let n_f = n_frets as f64;
        let per = style.period;

        let fx = |x: f64| if left_handed { board_w - x } else { x };
        (0..n_frets).fold(Diagram::empty(), |acc, i| {
            // For left-handed, fret labels are mirrored: fret 1 is near the right end.
            let fret_idx = if left_handed { n_frets - 1 - i } else { i };
            let name = display_note(offset + lowest + fret_idx as i32 + 1, note_names);
            // Center the label in the fret cell between fret i and fret i+1.
            let x_lo = fx(fret_pos(i as f64, n_f, edo_f, per) * board_w);
            let x_hi = fx(fret_pos((i + 1) as f64, n_f, edo_f, per) * board_w);
            let x = (x_lo + x_hi) / 2.0;
            acc + text(name.clone(), note_fs)
                .fc(label_color)
                .bold()
                .font_family(note_font(&name))
                .translate(x, label_y)
        })
    } else {
        Diagram::empty()
    };

    // ── Title (centered above the diagram) ────────────────────────────────────
    let title_text = if has_names {
        format!("{} {}", display_note(key, note_names), scale_name)
    } else {
        scale_name.to_string()
    };
    let title_font = note_font(&title_text);
    let title_y = if has_names {
        if has_open_dots {
            -hs * 1.6
        } else {
            -hs * 1.1
        }
    } else {
        -hs * 0.55
    };
    let title = text(title_text, hs * 0.52)
        .fc(label_color)
        .bold()
        .font_family(title_font)
        .translate(board_w / 2.0, title_y);

    title + string_markers + fret_markers + fretboard_body
}
