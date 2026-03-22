use crate::components::{Combobox, FretboardPreview, Modal, Select};
use crate::components::InstrumentPicker;
use crate::fretboard::{render_board, FretboardStyle};
use crate::models::{NotationSystem, Scale};
use crate::scala;
use crate::state::{DiagramMode, APP_STATE};
use crate::{audio, pitch_tracking, theory};
use dioxus::prelude::*;
use dioxus::document::eval;

static BRAVURA_FONT: Asset = asset!("/assets/Bravura.woff2");

/// Bravura font bytes embedded at compile-time for offline PDF generation.
/// Uses .woff (zlib-wrapped SFNT) rather than .woff2 (Brotli) because fontdb's
/// bundled ttf-parser can decompress woff natively without extra crate features.
static BRAVURA_BYTES: &[u8] = include_bytes!("../../assets/Bravura.woff");

/// Minimal Noto Sans subset (Latin A-Z, a-z, 0-9, punctuation) for PDF text
/// on WASM where system fonts are unavailable.  OFL-licensed, ~10 KB.
static NOTO_SANS_BYTES: &[u8] = include_bytes!("../../assets/NotoSans-Latin.ttf");

/// Encode bytes as base64 (used to pass binary PDF data through the JS eval bridge).
fn to_base64(bytes: &[u8]) -> String {
    const TABLE: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity((bytes.len() + 2) / 3 * 4);
    for chunk in bytes.chunks(3) {
        let b = [
            chunk[0] as u32,
            chunk.get(1).copied().unwrap_or(0) as u32,
            chunk.get(2).copied().unwrap_or(0) as u32,
        ];
        let n = (b[0] << 16) | (b[1] << 8) | b[2];
        out.push(TABLE[((n >> 18) & 63) as usize] as char);
        out.push(TABLE[((n >> 12) & 63) as usize] as char);
        out.push(if chunk.len() > 1 { TABLE[((n >> 6) & 63) as usize] as char } else { '=' });
        out.push(if chunk.len() > 2 { TABLE[(n & 63) as usize] as char } else { '=' });
    }
    out
}

/// Replace `width="100%"` in the SVG with explicit dimensions scaled to fit
/// ~500×700 PDF points (≈ A4 with margins), maintaining the diagram's aspect ratio.
/// hagoromo outputs a viewBox in abstract float units (e.g. `0 0 2.3 5.8`);
/// without scaling these up, usvg produces a ~2-point wide PDF page.
fn svg_with_explicit_size(svg: &str) -> String {
    let vb_dims = svg
        .split("viewBox=\"")
        .nth(1)
        .and_then(|s| s.split('"').next())
        .and_then(|s| {
            let v: Vec<f32> = s.split_whitespace().filter_map(|n| n.parse().ok()).collect();
            if v.len() == 4 { Some((v[2], v[3])) } else { None }
        });
    match vb_dims {
        Some((vw, vh)) if vw > 0.0 && vh > 0.0 => {
            const MAX_W: f32 = 500.0;
            const MAX_H: f32 = 700.0;
            let scale = (MAX_W / vw).min(MAX_H / vh);
            let w = vw * scale;
            let h = vh * scale;
            svg.replacen(
                "width=\"100%\"",
                &format!("width=\"{w:.2}\" height=\"{h:.2}\""),
                1,
            )
        }
        _ => svg.to_string(),
    }
}

/// Convert a WOFF1 font to raw SFNT bytes that fontdb/ttf-parser can load.
/// WOFF1 wraps each OpenType table in an optional zlib deflate stream; we
/// decompress each one and reconstruct a standard SFNT binary.
fn woff1_to_sfnt(woff: &[u8]) -> Option<Vec<u8>> {
    use std::convert::TryInto;

    if woff.len() < 44 { return None; }
    if &woff[0..4] != b"wOFF" { return None; }

    let sfnt_flavor = u32::from_be_bytes(woff[4..8].try_into().ok()?);
    let num_tables = u16::from_be_bytes(woff[12..14].try_into().ok()?) as usize;

    if woff.len() < 44 + num_tables * 20 { return None; }

    struct Entry { tag: [u8; 4], woff_off: usize, comp_len: usize, orig_len: usize, checksum: u32 }
    let mut tables: Vec<Entry> = Vec::with_capacity(num_tables);
    for i in 0..num_tables {
        let e = 44 + i * 20;
        tables.push(Entry {
            tag:      woff[e..e+4].try_into().ok()?,
            woff_off: u32::from_be_bytes(woff[e+4..e+8].try_into().ok()?)  as usize,
            comp_len: u32::from_be_bytes(woff[e+8..e+12].try_into().ok()?) as usize,
            orig_len: u32::from_be_bytes(woff[e+12..e+16].try_into().ok()?) as usize,
            checksum: u32::from_be_bytes(woff[e+16..e+20].try_into().ok()?),
        });
    }
    tables.sort_by(|a, b| a.tag.cmp(&b.tag));

    let n = num_tables as u16;
    let mut x = 1u16;
    while x * 2 <= n { x *= 2; }
    let search_range   = x * 16;
    let entry_selector = (x as f32).log2() as u16;
    let range_shift    = n * 16 - search_range;

    // Compute each table's position in the output (4-byte aligned after the header)
    let header_size = 12 + num_tables as u32 * 16;
    let mut sfnt_offsets: Vec<u32> = Vec::with_capacity(num_tables);
    let mut cursor = header_size;
    for t in &tables {
        sfnt_offsets.push(cursor);
        cursor += t.orig_len as u32;
        cursor = (cursor + 3) & !3;
    }

    let mut out = vec![0u8; cursor as usize];

    // Offset table
    out[0..4].copy_from_slice(&sfnt_flavor.to_be_bytes());
    out[4..6].copy_from_slice(&n.to_be_bytes());
    out[6..8].copy_from_slice(&search_range.to_be_bytes());
    out[8..10].copy_from_slice(&entry_selector.to_be_bytes());
    out[10..12].copy_from_slice(&range_shift.to_be_bytes());

    for (i, (t, &off)) in tables.iter().zip(sfnt_offsets.iter()).enumerate() {
        let d = 12 + i * 16;
        out[d..d+4].copy_from_slice(&t.tag);
        out[d+4..d+8].copy_from_slice(&t.checksum.to_be_bytes());
        out[d+8..d+12].copy_from_slice(&off.to_be_bytes());
        out[d+12..d+16].copy_from_slice(&(t.orig_len as u32).to_be_bytes());

        let src = woff.get(t.woff_off..t.woff_off + t.comp_len)?;
        let data: Vec<u8> = if t.comp_len < t.orig_len {
            miniz_oxide::inflate::decompress_to_vec_zlib(src).ok()?
        } else {
            src.to_vec()
        };
        let start = off as usize;
        out[start..start + data.len()].copy_from_slice(&data);
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn woff1_converts_and_loads() {
        let sfnt = woff1_to_sfnt(BRAVURA_BYTES).expect("woff1_to_sfnt should return Some");
        assert_eq!(&sfnt[0..4], &[0x00, 0x01, 0x00, 0x00], "SFNT must start with TrueType magic");

        let mut db = svg2pdf::usvg::fontdb::Database::new();
        let ids = db.load_font_source(svg2pdf::usvg::fontdb::Source::Binary(
            std::sync::Arc::new(sfnt),
        ));
        assert!(!ids.is_empty(), "fontdb must load at least one face from converted SFNT");

        let face_id = db.faces().find(|f| {
            f.families.iter().any(|(n, _)| n.to_lowercase().contains("bravura"))
        }).map(|f| f.id).expect("fontdb must find a Bravura face");

        // Verify the natural sign glyph (U+E261) is present using ttf_parser directly.
        let has_natural = db.with_face_data(face_id, |font_data, face_index| {
            ttf_parser::Face::parse(font_data, face_index)
                .ok()
                .and_then(|face| face.glyph_index('\u{E261}').map(|_| true))
        }).flatten().unwrap_or(false);
        assert!(has_natural, "Bravura face must have SMuFL natural sign (U+E261)");
    }

    #[test]
    fn noto_sans_subset_has_latin() {
        let mut db = svg2pdf::usvg::fontdb::Database::new();
        db.load_font_data(NOTO_SANS_BYTES.to_vec());
        let face = db.faces().next().expect("Noto Sans must load");
        assert!(
            face.families.iter().any(|(n, _)| n == "Noto Sans"),
            "font must register as 'Noto Sans'"
        );
        let has_a = db.with_face_data(face.id, |data, idx| {
            ttf_parser::Face::parse(data, idx)
                .ok()
                .and_then(|f| f.glyph_index('A').map(|_| true))
        }).flatten().unwrap_or(false);
        assert!(has_a, "Noto Sans subset must have Latin 'A'");
    }
}

/// Convert the current diagram SVG to a PDF, returning (filename, bytes).
fn build_pdf() -> Option<(String, Vec<u8>)> {
    let raw = build_svg()?;
    // Rewrite font-family values for the PDF rendering pipeline.
    //
    // Bravura.woff registers as "Bravura Text" in fontdb, so we correct that name.
    // For SMuFL spans: "Bravura Text" is primary, "Noto Sans" is Latin fallback.
    // For ASCII spans: "Noto Sans" is primary, "sans-serif" is generic fallback.
    //
    // On native, load_system_fonts() provides additional sans-serif coverage.
    // On WASM, Noto Sans (embedded 10 KB subset) is the only Latin source.
    let raw = raw.replace("font-family=\"Bravura, sans-serif\"", "font-family=\"Bravura Text, Noto Sans, sans-serif\"");
    let raw = raw.replace("font-family='Bravura, sans-serif'", "font-family='Bravura Text, Noto Sans, sans-serif'");
    let raw = raw.replace("font-family=\"Bravura\"", "font-family=\"Bravura Text, Noto Sans, sans-serif\"");
    let raw = raw.replace("font-family='Bravura'", "font-family='Bravura Text, Noto Sans, sans-serif'");
    let raw = raw.replace("font-family=\"sans-serif\"", "font-family=\"Noto Sans, sans-serif\"");
    let raw = raw.replace("font-family='sans-serif'", "font-family='Noto Sans, sans-serif'");
    let svg = svg_with_explicit_size(&raw);
    let filename = {
        let s = APP_STATE.read();
        s.current_scale()
            .map(|sc| format!("{}.pdf", sc.name.to_lowercase().replace(' ', "_")))
            .unwrap_or_else(|| "fretboard.pdf".to_string())
    };

    let mut options = svg2pdf::usvg::Options::default();
    options.fontdb_mut().load_system_fonts();
    if let Some(sfnt) = woff1_to_sfnt(BRAVURA_BYTES) {
        options.fontdb_mut().load_font_data(sfnt);
    }
    // Load embedded Noto Sans subset for Latin coverage (critical on WASM where
    // load_system_fonts() is a no-op).
    options.fontdb_mut().load_font_data(NOTO_SANS_BYTES.to_vec());
    options.fontdb_mut().set_sans_serif_family("Noto Sans");
    let tree = svg2pdf::usvg::Tree::from_str(&svg, &options).ok()?;
    let pdf = svg2pdf::to_pdf(
        &tree,
        svg2pdf::ConversionOptions::default(),
        svg2pdf::PageOptions::default(),
    )
    .ok()?;
    Some((filename, pdf))
}

/// Generate an SCL file for the current scale/chord, or None if nothing is selected.
/// Intervals are expressed as cumulative cents using the temperament's period.
fn build_scl() -> Option<(String, String)> {
    let state = APP_STATE.read();
    let settings = state.diagram_settings.clone();
    let edo = state.current_temperament().map(|t| t.divisions).unwrap_or(12);
    let period = state.current_temperament()
        .map(|t| t.period.0 as f64 / t.period.1 as f64)
        .unwrap_or(2.0);
    let step_cents = 1200.0 * period.log2() / edo as f64;

    let (name, intervals): (String, Vec<i32>) = match settings.mode {
        DiagramMode::Scale => {
            let sc = state.current_scale()?;
            (sc.name.clone(), sc.intervals.clone())
        }
        DiagramMode::Chord => {
            let ch = state.current_chord()?;
            (ch.name.clone(), ch.intervals.clone())
        }
    };
    drop(state);

    let mut cumulative = 0i32;
    let pitches: Vec<scala::Pitch> = intervals
        .iter()
        .map(|&steps| {
            cumulative += steps;
            scala::Pitch::Cents(cumulative as f64 * step_cents)
        })
        .collect();

    let scl = scala::SclFile { description: name.clone(), pitches };
    let filename = format!("{}.scl", name.to_lowercase().replace(' ', "_"));
    Some((filename, scl.to_string()))
}

/// Generate the current diagram SVG from app state, or None if no scale/tuning selected.
fn build_svg() -> Option<String> {
    let state = APP_STATE.read();
    let prefs = state.preferences.clone();
    let dark = state.effective_dark_mode();
    let settings = state.diagram_settings.clone();
    let maybe_scale = state.current_scale().cloned();
    let maybe_tuning = state.current_tuning().cloned();
    let edo = state.current_temperament().map(|t| t.divisions).unwrap_or(12);
    let note_names: Vec<String> = state
        .current_notation_system()
        .map(|ns| ns.note_names_for_key(edo, settings.key_natural_idx, settings.key_accidental_idx))
        .unwrap_or_default();
    let period = state.current_temperament()
        .map(|t| t.period.0 as f64 / t.period.1 as f64)
        .unwrap_or(2.0);
    drop(state);

    let (scale, tuning) = match (maybe_scale, maybe_tuning) {
        (Some(s), Some(t)) => (s, t),
        _ => return None,
    };
    let n_frets_h = {
        let n = (edo as f64 * 4_f64.ln() / period.ln()).round() as u32;
        n.clamp(8, 72)
    };
    let default_vs = settings.vertical_spacing as f64 / 1000.0;
    let vs_h = default_vs * 24.0 / n_frets_h as f64;

    let fret_style = FretboardStyle {
        display_markers_on_frets: settings.display_markers,
        fret_offset: settings.fret_offset,
        num_frets: if settings.horizontal { n_frets_h } else { settings.num_frets },
        vertical_spacing: if settings.horizontal { vs_h } else { default_vs },
        horizontal_spacing: settings.horizontal_spacing as f64 / 1000.0,
        horizontal: settings.horizontal,
        edo,
        period,
    };
    let note_names_ref: Option<&[String]> = if note_names.is_empty() { None } else { Some(&note_names) };
    Some(render_board(
        &prefs, &scale.name, settings.key as i32, &scale,
        tuning.skip_frets, &tuning, &fret_style, note_names_ref,
        &BRAVURA_FONT.to_string(), &[], &[], dark,
    ))
}

#[component]
pub fn Home() -> Element {
    let mut drawer_open = use_signal(|| false);
    let mut is_mobile = use_signal(|| false);
    let mut is_landscape = use_signal(|| false);
    let mut show_instrument_picker = use_signal(|| false);
    let mut show_export_modal = use_signal(|| false);
    let mut is_playing = use_signal(|| false);
    let mut playing_degrees: Signal<Vec<usize>> = use_signal(Vec::new);
    let mut playing_steps: Signal<Vec<i32>> = use_signal(Vec::new);
    let mut mic_active = use_signal(|| false);
    let mut mic_degrees: Signal<Vec<usize>> = use_signal(Vec::new);
    let mut mic_steps: Signal<Vec<i32>> = use_signal(Vec::new);

    let do_export_svg = move |_| {
        show_export_modal.set(false);
        let Some(svg) = build_svg() else { return };
        let filename = {
            let s = APP_STATE.read();
            s.current_scale()
                .map(|sc| format!("{}.svg", sc.name.to_lowercase().replace(' ', "_")))
                .unwrap_or_else(|| "fretboard.svg".to_string())
        };
        let escaped = svg.replace('\\', "\\\\").replace('`', "\\`").replace("${", "\\${");
        let js = format!(
            "const b=new Blob([`{escaped}`],{{type:'image/svg+xml'}});\
             const u=URL.createObjectURL(b);\
             const a=document.createElement('a');\
             a.href=u;a.download={filename:?};a.click();\
             URL.revokeObjectURL(u);"
        );
        let _ = eval(&js);
    };

    let do_export_pdf = move |_| {
        show_export_modal.set(false);
        let Some((filename, bytes)) = build_pdf() else { return };
        let b64 = to_base64(&bytes);
        let js = format!(
            "const s=atob({b64:?});\
             const b=new Uint8Array(s.length);\
             for(let i=0;i<s.length;i++)b[i]=s.charCodeAt(i);\
             const bl=new Blob([b],{{type:'application/pdf'}});\
             const u=URL.createObjectURL(bl);\
             const a=document.createElement('a');\
             a.href=u;a.download={filename:?};a.click();\
             URL.revokeObjectURL(u);"
        );
        let _ = eval(&js);
    };

    let do_export_scl = move |_| {
        show_export_modal.set(false);
        let Some((filename, content)) = build_scl() else { return };
        let escaped = content.replace('\\', "\\\\").replace('`', "\\`").replace("${", "\\${");
        let js = format!(
            "const b=new Blob([`{escaped}`],{{type:'text/plain'}});\
             const u=URL.createObjectURL(b);\
             const a=document.createElement('a');\
             a.href=u;a.download={filename:?};a.click();\
             URL.revokeObjectURL(u);"
        );
        let _ = eval(&js);
    };

    let state = APP_STATE.read();
    let has_instrument = state.selected_instrument_idx.is_some();
    let instrument_label = state.current_instrument()
        .map(|i| format!("{} ({})", i.name, i.temperament_name))
        .unwrap_or_default();
    let instrument_type_label = state.current_instrument()
        .map(|i| i.instrument_type.clone())
        .unwrap_or_default();

    let sel_tuning_idx = state.selected_tuning_idx;
    let sel_scale_idx = state.selected_scale_idx;
    let sel_chord_idx = state.selected_chord_idx;
    let settings = state.diagram_settings.clone();
    let concert_hz = state.preferences.concert_hz;
    let concert_octave = state.preferences.concert_octave;
    let is_scale_mode = matches!(settings.mode, DiagramMode::Scale);
    let is_horizontal = settings.horizontal;
    let effective_horizontal = if *is_mobile.read() { *is_landscape.read() } else { is_horizontal };

    let maybe_item: Option<Scale> = if is_scale_mode {
        state.current_scale().cloned()
    } else {
        state.current_chord().map(|c| Scale {
            temperament_name: String::new(),
            name: c.name.clone(),
            intervals: c.intervals.clone(),
        })
    };
    let playback_octave = settings.playback_octave;
    let (play_freqs, play_abs_steps): (Vec<f64>, Vec<i32>) = state
        .current_temperament()
        .zip(maybe_item.as_ref())
        .map(|(temp, item)| {
            let period_ratio = temp.period.0 as f64 / temp.period.1 as f64;
            let step0_hz = state.current_tuning()
                .map(|tu| tu.step0_hz(concert_hz, concert_octave, temp.divisions, temp.period))
                .unwrap_or(440.0);
            let root_hz = step0_hz * period_ratio.powi(playback_octave);
            let freqs = theory::scale_to_hz_sequence(item, settings.key as i32, temp.divisions, temp.period, root_hz);
            let abs = theory::scale_absolute_steps(item, settings.key as i32)
                .into_iter()
                .map(|s| s + (temp.divisions as i32) * playback_octave)
                .collect();
            (freqs, abs)
        })
        .unwrap_or_default();

    let tuning_options: Vec<(usize, String)> = state.compatible_tunings()
        .iter()
        .enumerate()
        .map(|(i, (_, tu))| (i, tu.name.clone()))
        .collect();

    let scale_options: Vec<(usize, String)> = state
        .current_temperament()
        .map(|t| {
            let mut opts: Vec<(usize, String, usize)> = state.scales.iter()
                .filter(|s| s.temperament_name == t.name)
                .enumerate()
                .map(|(i, s)| (i, s.name.clone(), s.intervals.len()))
                .collect();
            opts.sort_by(|a, b| a.2.cmp(&b.2).then(a.1.cmp(&b.1)));
            opts.into_iter().map(|(i, name, _)| (i, name)).collect()
        })
        .unwrap_or_default();

    let chord_options: Vec<(usize, String)> = state
        .current_temperament()
        .map(|t| {
            state.chords.iter()
                .filter(|c| c.temperament_name == t.name)
                .enumerate()
                .map(|(i, c)| (i, c.name.clone()))
                .collect()
        })
        .unwrap_or_default();

    let divisions = state
        .current_temperament()
        .map(|t| t.divisions)
        .unwrap_or(12);
    let notation_system: Option<NotationSystem> = state
        .current_notation_system()
        .cloned();
    let notation_options: Vec<(String, String)> = state
        .current_temperament()
        .map(|t| {
            state.notation_systems.iter()
                .filter(|ns| ns.temperament_name == t.name)
                .map(|ns| (ns.name.clone(), if ns.name.is_empty() { "Default".to_string() } else { ns.name.clone() }))
                .collect()
        })
        .unwrap_or_default();
    let current_ns_name = notation_system.as_ref().map(|ns| ns.name.clone()).unwrap_or_default();
    let current_temp_name = state.current_temperament().map(|t| t.name.clone()).unwrap_or_default();
    let effective_nat_idx: Option<usize> = settings.key_natural_idx
        .or_else(|| notation_system.as_ref()
            .and_then(|ns| ns.naturals.iter().position(|n| n.degree == settings.key)))
        .or(Some(0));
    let concert_ref_name = notation_system.as_ref()
        .and_then(|ns| ns.naturals.iter().find(|n| n.degree == 0))
        .map(|n| n.name.clone())
        .unwrap_or_else(|| "Step 0".to_string());
    let concert_label = format!("{}{}", concert_ref_name, concert_octave);
    drop(state);

    let has_freqs = !play_freqs.is_empty();
    let n_degrees = play_freqs.len().saturating_sub(1).max(1);
    let on_play = {
        let freqs = play_freqs;
        let abs_steps = play_abs_steps;
        move |_| {
            if is_playing() || freqs.is_empty() { return; }
            let f = freqs.clone();
            let steps = abs_steps.clone();
            spawn(async move {
                is_playing.set(true);
                if is_scale_mode {
                    let mut pb = audio::start_scale(&f);
                    while let Some(note_idx) = pb.next_note().await {
                        if let Some(&step) = steps.get(note_idx) {
                            playing_steps.set(vec![step]);
                        }
                    }
                    playing_steps.set(vec![]);
                } else {
                    audio::play_chord(&f).await;
                }
                is_playing.set(false);
            });
        }
    };

    let on_mic_toggle = move |_| {
        if mic_active() {
            let _ = eval(pitch_tracking::mic_web::MIC_STOP_JS);
            mic_active.set(false);
            mic_degrees.set(vec![]);
            mic_steps.set(vec![]);
        } else {
            mic_active.set(true);
            let detector_kind = APP_STATE.read().preferences.pitch_detector.clone();
            spawn(async move {
                let mut stream = pitch_tracking::MicStream::start(4096);
                let mut detector: Box<dyn pitch_tracking::PitchDetector> = match detector_kind {
                    crate::models::PitchDetectorKind::Yin =>
                        Box::new(pitch_tracking::YinDetector::new(0.15, 50.0, 4000.0)),
                    crate::models::PitchDetectorKind::IterF0 =>
                        Box::new(pitch_tracking::IterF0Detector::new(6, 8, 70.0, 2000.0, 0.15)),
                };
                let mut holdoff_degs: std::collections::HashMap<usize, u8> =
                    std::collections::HashMap::new();
                let mut holdoff_steps: std::collections::HashMap<i32, u8> =
                    std::collections::HashMap::new();
                const HOLDOFF_FRAMES: u8 = 12;

                while let Some(event) = stream.next_event().await {
                    if !mic_active() { break; }
                    match event {
                        pitch_tracking::MicEvent::Samples { rate, data } => {
                            let pitches = detector.detect(&data, rate);
                            let state = APP_STATE.read();
                            let concert_hz = state.preferences.concert_hz;

                            let (fresh_degs, fresh_steps): (Vec<usize>, Vec<i32>) =
                                if let (Some(temp), Some(scale), Some(tuning)) =
                                    (state.current_temperament(), state.current_scale(), state.current_tuning())
                                {
                                    let key = state.diagram_settings.key as i32;
                                    let step0_hz = tuning.step0_hz(
                                        concert_hz,
                                        state.preferences.concert_octave,
                                        temp.divisions,
                                        temp.period,
                                    );
                                    let period_ratio =
                                        temp.period.0 as f64 / temp.period.1 as f64;
                                    let lowest_step = tuning.string_tunings.iter().min().copied().unwrap_or(0);
                                    let lowest_hz = step0_hz * period_ratio.powf(lowest_step as f64 / temp.divisions as f64);
                                    let lowest_abs = (temp.divisions as f64
                                        * (lowest_hz / concert_hz).ln()
                                        / period_ratio.ln())
                                        .round() as i32;
                                    let offset = lowest_step - lowest_abs;
                                    let steps = pitches
                                        .iter()
                                        .filter(|p| p.confidence > 0.1)
                                        .filter_map(|p| {
                                            pitch_tracking::hz_to_absolute_step(
                                                p.hz,
                                                temp.divisions,
                                                temp.period,
                                                concert_hz,
                                                key,
                                                &scale.intervals,
                                                50.0,
                                            )
                                            .map(|s| s + offset)
                                        })
                                        .collect();
                                    (vec![], steps)
                                } else {
                                    (vec![], vec![])
                                };
                            drop(state);

                            holdoff_degs.retain(|_, c| { if *c == 0 { false } else { *c -= 1; true } });
                            holdoff_steps.retain(|_, c| { if *c == 0 { false } else { *c -= 1; true } });
                            for &d in &fresh_degs { holdoff_degs.insert(d, HOLDOFF_FRAMES); }
                            for &s in &fresh_steps { holdoff_steps.insert(s, HOLDOFF_FRAMES); }
                            mic_degrees.set(holdoff_degs.keys().copied().collect());
                            mic_steps.set(holdoff_steps.keys().copied().collect());
                        }
                        pitch_tracking::MicEvent::Error(_) | pitch_tracking::MicEvent::Stopped => {
                            mic_active.set(false);
                            mic_degrees.set(vec![]);
                            mic_steps.set(vec![]);
                            break;
                        }
                        _ => {}
                    }
                }
            });
        }
    };

    use_effect(move || {
        spawn(async move {
            let mut e = eval(r#"
                function sendOrientation() {
                    dioxus.send({type: 'orientation', mobile: window.innerWidth <= 768, landscape: window.innerWidth > window.innerHeight});
                }
                sendOrientation();
                window.addEventListener('resize', sendOrientation);
                window.addEventListener('orientationchange', sendOrientation);
                // Swipe on drawer handle
                let startY = 0;
                document.addEventListener('touchstart', function(e) {
                    if (e.target.closest('.drawer-handle')) startY = e.touches[0].clientY;
                }, {passive: true});
                document.addEventListener('touchend', function(e) {
                    if (!e.target.closest('.drawer-handle')) return;
                    var delta = e.changedTouches[0].clientY - startY;
                    if (delta < -50) dioxus.send({type: 'swipe', dir: 'open'});
                    else if (delta > 50) dioxus.send({type: 'swipe', dir: 'close'});
                }, {passive: true});
            "#);
            loop {
                match e.recv().await {
                    Ok(serde_json::Value::Object(obj)) => {
                        match obj.get("type").and_then(|t| t.as_str()) {
                            Some("orientation") => {
                                let mobile = obj.get("mobile").and_then(|v| v.as_bool()).unwrap_or(false);
                                let landscape = obj.get("landscape").and_then(|v| v.as_bool()).unwrap_or(false);
                                is_mobile.set(mobile);
                                is_landscape.set(landscape);
                            }
                            Some("swipe") => {
                                match obj.get("dir").and_then(|v| v.as_str()) {
                                    Some("open") => drawer_open.set(true),
                                    Some("close") => drawer_open.set(false),
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => break,
                }
            }
        });
    });

    let effective_degrees = mic_degrees.read().clone();
    let effective_steps = {
        let mut steps = playing_steps.read().clone();
        steps.extend(mic_steps.read().iter().copied());
        steps
    };

    rsx! {
        div { class: if effective_horizontal { "home-layout home-layout--h" } else { "home-layout" },

            // ── Controls panel ──────────────────────────────────────────────
            div {
                class: if *drawer_open.read() { "controls-panel drawer-open" } else { "controls-panel" },

                // Drawer handle — visible on mobile only
                div {
                    class: "drawer-handle",
                    onclick: move |_| {
                        let open = *drawer_open.read();
                        drawer_open.set(!open);
                    },
                    div { class: "drawer-pill" }
                    span { class: "drawer-label",
                        if *drawer_open.read() { "Settings ▾" } else { "Settings ▴" }
                    }
                }

                div { class: "controls-body",

                    // Instrument selector
                    div { class: "form-group",
                        label { class: "form-label", "Instrument" }
                        button {
                            class: "instrument-selector",
                            onclick: move |_| show_instrument_picker.set(true),
                            if has_instrument {
                                span { class: "instrument-selector-type", "{instrument_type_label}" }
                                span { class: "instrument-selector-label", "{instrument_label}" }
                            } else {
                                span { class: "instrument-selector-label instrument-selector-label--empty", "Select an instrument..." }
                            }
                            span { class: "instrument-selector-chevron",
                                dangerous_inner_html: r#"<svg width="12" height="12" viewBox="0 0 12 12" aria-hidden="true"><path fill="currentColor" d="M6 8L1 3h10z"/></svg>"#,
                            }
                        }
                    }

                    if has_instrument {
                        // Tuning
                        div { class: "form-group",
                            if !effective_horizontal { label { class: "form-label", "Tuning" } }
                            if tuning_options.is_empty() {
                                p { class: "form-empty", "No compatible tunings for this instrument." }
                            } else {
                                Select {
                                    value: sel_tuning_idx.to_string(),
                                    options: tuning_options.iter().map(|(idx, name)| (idx.to_string(), name.clone())).collect(),
                                    on_change: move |val: String| {
                                        if let Ok(idx) = val.parse::<usize>() {
                                            APP_STATE.write().selected_tuning_idx = idx;
                                        }
                                    },
                                }
                            }
                        }

                        // Notation system (only when multiple are available)
                        if notation_options.len() > 1 {
                            div { class: "form-group",
                                label { class: "form-label", "Notation" }
                                Select {
                                    value: current_ns_name.clone(),
                                    options: notation_options,
                                    on_change: {
                                        let temp_name = current_temp_name.clone();
                                        move |val: String| {
                                            APP_STATE.write().preferences.notation_system_prefs
                                                .insert(temp_name.clone(), val);
                                        }
                                    },
                                }
                            }
                        }

                        // Diagram type
                        div { class: "form-group",
                            if !effective_horizontal { label { class: "form-label", "Diagram Type" } }
                            div { class: "segmented-control",
                                button {
                                    class: if is_scale_mode { "seg-btn active" } else { "seg-btn" },
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.mode = DiagramMode::Scale;
                                    },
                                    "Scale"
                                }
                                button {
                                    class: if !is_scale_mode { "seg-btn active" } else { "seg-btn" },
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.mode = DiagramMode::Chord;
                                    },
                                    "Chord"
                                }
                            }
                        }

                        // Orientation
                        div { class: "form-group hide-mobile",
                            if !effective_horizontal { label { class: "form-label", "Orientation" } }
                            div { class: "segmented-control",
                                button {
                                    class: if !is_horizontal { "seg-btn active" } else { "seg-btn" },
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.horizontal = false;
                                    },
                                    "Vertical"
                                }
                                button {
                                    class: if is_horizontal { "seg-btn active" } else { "seg-btn" },
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.horizontal = true;
                                    },
                                    "Horizontal"
                                }
                            }
                        }

                        // Scale or Chord selection
                        if is_scale_mode {
                            div { class: "form-group",
                                label { class: "form-label", "Scale" }
                                if scale_options.is_empty() {
                                    p { class: "form-empty", "No scales defined for this temperament." }
                                } else {
                                    Combobox {
                                        value: sel_scale_idx.to_string(),
                                        options: scale_options.iter().map(|(idx, name)| (idx.to_string(), name.clone())).collect(),
                                        on_change: move |val: String| {
                                            if let Ok(idx) = val.parse::<usize>() {
                                                APP_STATE.write().selected_scale_idx = idx;
                                            }
                                        },
                                    }
                                }
                            }
                        } else {
                            div { class: "form-group",
                                label { class: "form-label", "Chord" }
                                if chord_options.is_empty() {
                                    p { class: "form-empty", "No chords defined for this temperament." }
                                } else {
                                    Combobox {
                                        value: sel_chord_idx.to_string(),
                                        options: chord_options.iter().map(|(idx, name)| (idx.to_string(), name.clone())).collect(),
                                        on_change: move |val: String| {
                                            if let Ok(idx) = val.parse::<usize>() {
                                                APP_STATE.write().selected_chord_idx = idx;
                                            }
                                        },
                                    }
                                }
                            }
                        }

                        // Key + Fret Offset (side by side)
                        div { class: "form-row",
                            div { class: "form-group",
                                label { class: "form-label", "Key" }
                                if let Some(ns) = notation_system.as_ref() {
                                    // Natural picker row
                                    div { class: "key-picker-naturals",
                                        for (nat_idx, nat) in ns.naturals.iter().enumerate() {
                                            {
                                                let nat_name = nat.name.clone();
                                                let is_selected = effective_nat_idx == Some(nat_idx);
                                                let is_smufl = nat_name.chars().any(|c| c as u32 >= 0xE000);
                                                rsx! {
                                                    button {
                                                        class: if is_selected { "key-btn key-btn--active" } else { "key-btn" },
                                                        style: if is_smufl { "font-family: 'Bravura', sans-serif;" } else { "" },
                                                        onclick: move |_| {
                                                            let mut s = APP_STATE.write();
                                                            s.diagram_settings.key_natural_idx = Some(nat_idx);
                                                            let temp = s.current_temperament().cloned();
                                                            if let Some(temp) = temp {
                                                                let ns = s.current_notation_system().cloned();
                                                                if let Some(ns) = ns {
                                                                    let nat = ns.naturals.get(nat_idx).cloned();
                                                                    let acc = s.diagram_settings.key_accidental_idx
                                                                        .and_then(|ai| ns.accidentals.get(ai).cloned());
                                                                    if let Some(nat) = nat {
                                                                        s.diagram_settings.key = ns.resolve_degree(
                                                                            &nat, acc.as_ref(), temp.divisions
                                                                        );
                                                                    }
                                                                }
                                                            }
                                                        },
                                                        "{nat_name}"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    // Accidental picker row
                                    div { class: "key-picker-accidentals",
                                        // Natural/no-accidental button
                                        {
                                            let is_selected = settings.key_accidental_idx.is_none();
                                            rsx! {
                                                button {
                                                    class: if is_selected { "key-btn key-btn--active" } else { "key-btn" },
                                                    style: "font-family: 'Bravura', sans-serif;",
                                                    onclick: move |_| {
                                                        let mut s = APP_STATE.write();
                                                        s.diagram_settings.key_accidental_idx = None;
                                                        let temp = s.current_temperament().cloned();
                                                        if let Some(temp) = temp {
                                                            let ns = s.current_notation_system().cloned();
                                                            if let Some(ns) = ns {
                                                                let key = s.diagram_settings.key;
                                                                let ni = s.diagram_settings.key_natural_idx
                                                                    .or_else(|| ns.naturals.iter().position(|n| n.degree == key))
                                                                    .unwrap_or(0);
                                                                s.diagram_settings.key_natural_idx = Some(ni);
                                                                if let Some(nat) = ns.naturals.get(ni) {
                                                                    s.diagram_settings.key = ns.resolve_degree(nat, None, temp.divisions);
                                                                }
                                                            }
                                                        }
                                                    },
                                                    "\u{E261}"
                                                }
                                            }
                                        }
                                        for (acc_idx, acc) in ns.accidentals.iter().enumerate() {
                                            {
                                                let acc_name = acc.name.clone();
                                                let is_selected = settings.key_accidental_idx == Some(acc_idx);
                                                let is_smufl = acc_name.chars().any(|c| c as u32 >= 0xE000);
                                                rsx! {
                                                    button {
                                                        class: if is_selected { "key-btn key-btn--active" } else { "key-btn" },
                                                        style: if is_smufl { "font-family: 'Bravura', sans-serif;" } else { "" },
                                                        onclick: move |_| {
                                                            let mut s = APP_STATE.write();
                                                            s.diagram_settings.key_accidental_idx = Some(acc_idx);
                                                            let temp = s.current_temperament().cloned();
                                                            if let Some(temp) = temp {
                                                                let ns = s.current_notation_system().cloned();
                                                                if let Some(ns) = ns {
                                                                    let key = s.diagram_settings.key;
                                                                    let ni = s.diagram_settings.key_natural_idx
                                                                        .or_else(|| ns.naturals.iter().position(|n| n.degree == key))
                                                                        .unwrap_or(0);
                                                                    s.diagram_settings.key_natural_idx = Some(ni);
                                                                    let acc = ns.accidentals.get(acc_idx).cloned();
                                                                    if let Some(nat) = ns.naturals.get(ni) {
                                                                        s.diagram_settings.key = ns.resolve_degree(
                                                                            nat, acc.as_ref(), temp.divisions
                                                                        );
                                                                    }
                                                                }
                                                            }
                                                        },
                                                        "{acc_name}"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    // Fallback: numeric input when no notation system
                                    input {
                                        class: "form-input",
                                        r#type: "number",
                                        min: "0",
                                        max: "{divisions - 1}",
                                        value: "{settings.key}",
                                        onchange: move |e| {
                                            if let Ok(v) = e.value().parse::<u32>() {
                                                APP_STATE.write().diagram_settings.key = v;
                                            }
                                        }
                                    }
                                }
                            }
                            div { class: "form-group",
                                label { class: "form-label", "Fret Offset" }
                                input {
                                    class: "form-input",
                                    r#type: "number",
                                    min: "0",
                                    value: "{settings.fret_offset}",
                                    onchange: move |e| {
                                        if let Ok(v) = e.value().parse::<u32>() {
                                            APP_STATE.write().diagram_settings.fret_offset = v;
                                        }
                                    }
                                }
                            }
                        }

                        // Concert pitch
                        div { class: "form-group",
                            label { class: "form-label", "Concert Pitch" }
                            div { class: "form-row",
                                div { class: "form-group",
                                    label { class: "form-label", "{concert_label} =" }
                                    input {
                                        class: "form-input",
                                        r#type: "number",
                                        step: "0.01",
                                        min: "1.0",
                                        max: "20000.0",
                                        value: "{concert_hz}",
                                        oninput: move |e| {
                                            if let Ok(v) = e.value().parse::<f64>() {
                                                if v > 0.0 {
                                                    APP_STATE.write().preferences.concert_hz = v;
                                                }
                                            }
                                        }
                                    }
                                    span { "Hz" }
                                }
                                div { class: "form-group",
                                    label { class: "form-label", "Octave" }
                                    input {
                                        class: "form-input",
                                        r#type: "number",
                                        min: "-2",
                                        max: "9",
                                        value: "{concert_octave}",
                                        oninput: move |e| {
                                            if let Ok(v) = e.value().parse::<i32>() {
                                                APP_STATE.write().preferences.concert_octave = v;
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // Action buttons
                        div { class: "controls-actions",
                            button {
                                class: "btn btn-outline btn-full",
                                onclick: move |_| show_export_modal.set(true),
                                "↓ Export"
                            }
                        }
                    } else {
                        // Empty state when no instrument is selected
                        div { class: "empty-state instrument-empty-state",
                            span { class: "empty-icon", "🎸" }
                            p { "No instrument selected. Add an instrument to get started." }
                            button {
                                class: "btn btn-primary",
                                onclick: move |_| show_instrument_picker.set(true),
                                "Add Instrument"
                            }
                        }
                    }
                }
            }

            // ── Preview panel ───────────────────────────────────────────────
            div { class: "preview-panel",
                div { class: "preview-header",
                    div { class: "preview-header-info",
                        h2 { class: "preview-title",
                            {
                                let s = APP_STATE.read();
                                let temp_name = s.current_temperament()
                                    .map(|t| t.name.clone())
                                    .unwrap_or_default();
                                let item_name = if is_scale_mode {
                                    s.current_scale().map(|sc| sc.name.clone())
                                } else {
                                    s.current_chord().map(|ch| ch.name.clone())
                                }.unwrap_or_default();
                                format!("{} · {}", temp_name, item_name)
                            }
                        }
                        span { class: "preview-subtitle",
                            {
                                let s = APP_STATE.read();
                                s.current_instrument()
                                    .map(|i| {
                                        let tuning_name = s.current_tuning()
                                            .map(|t| t.name.clone())
                                            .unwrap_or_default();
                                        format!("{} — {}", i.name, tuning_name)
                                    })
                                    .unwrap_or_default()
                            }
                        }
                    }
                    div { class: "preview-header-actions",
                        if has_freqs {
                            div { class: "octave-selector",
                                button {
                                    class: "btn-octave",
                                    title: "Lower octave",
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.playback_octave -= 1;
                                    },
                                    "−"
                                }
                                span { class: "octave-label",
                                    {if playback_octave == 0 { "Oct 0".to_string() } else { format!("Oct {:+}", playback_octave) }}
                                }
                                button {
                                    class: "btn-octave",
                                    title: "Higher octave",
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.playback_octave += 1;
                                    },
                                    "+"
                                }
                            }
                            button {
                                class: if is_playing() { "btn-play playing" } else { "btn-play" },
                                title: if is_playing() { "Playing…" } else { "Play scale" },
                                onclick: on_play,
                                if is_playing() { "■" } else { "▶" }
                            }
                        }
                        button {
                            class: if mic_active() { "btn-mic listening" } else { "btn-mic" },
                            title: if mic_active() { "Stop listening" } else { "Listen to mic" },
                            onclick: on_mic_toggle,
                            if mic_active() { "■" } else { "🎤" }
                        }
                    }
                }
                div { class: "preview-body",
                    FretboardPreview {
                        playing_degrees: effective_degrees.clone(),
                        playing_steps: effective_steps.clone(),
                        horizontal_override: Some(effective_horizontal),
                    }
                }
            }

        }

        // ── Instrument picker modal ─────────────────────────────────────
        InstrumentPicker { show: show_instrument_picker }

        // ── Export modal ────────────────────────────────────────────────
        Modal {
            show: show_export_modal,
            title: "Export".to_string(),
            div { class: "export-format-list",
                button {
                    class: "export-format-btn",
                    onclick: do_export_svg,
                    div { class: "export-format-icon", "SVG" }
                    div { class: "export-format-label", "SVG Image" }
                    div { class: "export-format-desc", "Fretboard diagram as a scalable vector graphic" }
                }
                button {
                    class: "export-format-btn",
                    onclick: do_export_pdf,
                    div { class: "export-format-icon", "PDF" }
                    div { class: "export-format-label", "PDF Document" }
                    div { class: "export-format-desc", "Fretboard diagram as a vector PDF" }
                }
                button {
                    class: "export-format-btn",
                    onclick: do_export_scl,
                    div { class: "export-format-icon", "SCL" }
                    div { class: "export-format-label", "Scala Scale File" }
                    div { class: "export-format-desc", "Current scale/chord in .scl format" }
                }
            }
        }
    }
}
