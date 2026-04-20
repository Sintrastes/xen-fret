use crate::state::{AppState, DiagramMode};
use fretboard_diagrams::{
    render_board, DegreeLabel, FretMarkerStyle, FretStyle, FretboardStyle, ScaleDotStyle,
    TitleStyle,
};
use xen_theory::scala;

/// Generate the current diagram SVG from app state, or None if no scale/tuning is selected.
/// `font_path` is the path/URL to the Bravura font (platform-specific).
pub fn build_svg(state: &AppState, font_path: &str) -> Option<String> {
    let prefs = &state.preferences;
    let dark = state.effective_dark_mode();
    let settings = &state.diagram_settings.fretboard_style;
    let maybe_scale = state.current_scale().cloned();
    let maybe_tuning = state.current_tuning().cloned();
    let left_handed = state
        .current_instrument()
        .and_then(|i| i.left_handed)
        .unwrap_or(prefs.left_handed);
    let edo = state
        .current_temperament()
        .map(|t| t.divisions)
        .unwrap_or(12);
    let note_names: Vec<String> = state
        .current_notation_system()
        .map(|ns| {
            ns.note_names_for_key(
                edo,
                state.diagram_settings.key_natural_idx,
                state.diagram_settings.key_accidental_idx,
            )
        })
        .unwrap_or_default();
    let period = state
        .current_temperament()
        .map(|t| t.period_f64())
        .unwrap_or(2.0);

    let (scale, tuning) = match (maybe_scale, maybe_tuning) {
        (Some(s), Some(t)) => (s, t),
        _ => return None,
    };
    let n_frets_h = {
        let n = (edo as f64 * 4_f64.ln() / period.ln()).round() as u32;
        n.clamp(8, 72)
    };
    let default_vs = settings.vertical_spacing as f64;
    let vs_h = default_vs * 24.0 / n_frets_h as f64;

    let fret_style = FretboardStyle {
        colors: prefs.diagram_colors(dark),
        scale_dots: settings.scale_dots,
        display_string_names: settings.display_string_names,
        fret_markers: FretMarkerStyle::None,
        title: settings.title,
        fret: settings.fret,
        fret_offset: settings.fret_offset,
        num_frets: if settings.horizontal {
            n_frets_h
        } else {
            settings.num_frets
        },
        vertical_spacing: if settings.horizontal {
            vs_h
        } else {
            default_vs
        },
        horizontal_spacing: settings.horizontal_spacing as f64,
        horizontal: settings.horizontal,
        left_handed,
    };

    let note_names_ref: Option<&[String]> = if note_names.is_empty() {
        None
    } else {
        Some(&note_names)
    };
    Some(render_board(
        state.diagram_settings.key as i32,
        &scale,
        tuning.skip_frets,
        &tuning,
        &fret_style,
        note_names_ref,
        font_path,
        &[],
        &[],
    ))
}

/// Generate an SCL file for the current scale/chord.
/// Returns `(filename, scl_contents)`, or None if nothing is selected.
pub fn build_scl(state: &AppState) -> Option<(String, String)> {
    let edo = state
        .current_temperament()
        .map(|t| t.divisions)
        .unwrap_or(12);
    let period = state
        .current_temperament()
        .map(|t| t.period_f64())
        .unwrap_or(2.0);
    let step_cents = 1200.0 * period.log2() / edo as f64;

    let (name, intervals): (String, Vec<i32>) = match state.diagram_settings.mode {
        DiagramMode::Scale => {
            let sc = state.current_scale()?;
            (sc.name.clone(), sc.intervals.clone())
        }
        DiagramMode::Chord => {
            let ch = state.current_chord()?;
            (ch.name.clone(), ch.intervals.clone())
        }
    };

    let mut cumulative = 0i32;
    let pitches: Vec<scala::Pitch> = intervals
        .iter()
        .map(|&steps| {
            cumulative += steps;
            scala::Pitch::Cents(cumulative as f64 * step_cents)
        })
        .collect();

    let scl = scala::SclFile {
        description: name.clone(),
        pitches,
    };
    let filename = format!("{}.scl", name.to_lowercase().replace(' ', "_"));
    Some((filename, scl.to_string()))
}
