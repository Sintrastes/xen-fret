use dioxus::prelude::*;

use crate::fretboard::{render_board, FretboardStyle};
use crate::models::Scale;
use crate::state::{APP_STATE, DiagramMode};

// Ensure Bravura is bundled and served. asset!() registers the file with the
// dev server and gives a content-hashed URL in release builds.
static BRAVURA_FONT: Asset = asset!("/assets/Bravura.woff2");

#[component]
pub fn FretboardPreview(
    playing_degrees: ReadOnlySignal<Vec<usize>>,
    playing_steps: ReadOnlySignal<Vec<i32>>,
    horizontal_override: Option<bool>,
) -> Element {
    let playing_degs = playing_degrees.read().clone();
    let playing_stps = playing_steps.read().clone();
    let state = APP_STATE.read();

    let prefs = state.preferences.clone();
    let settings = state.diagram_settings.clone();
    let dark = state.effective_dark_mode();
    let is_horizontal = horizontal_override.unwrap_or(settings.horizontal);

    // Resolve the current item (scale or chord) as a Scale for rendering.
    // Both types have identical fields so the conversion is free.
    let maybe_item: Option<Scale> = match settings.mode {
        DiagramMode::Scale => state.current_scale().cloned(),
        DiagramMode::Chord => state.current_chord().map(|c| Scale {
            temperament_name: String::new(),
            name: c.name.clone(),
            intervals: c.intervals.clone(),
        }),
    };
    let maybe_tuning = state.current_tuning().cloned();
    // Resolve effective handedness: instrument override takes priority over global preference.
    let left_handed = state.current_instrument()
        .and_then(|i| i.left_handed)
        .unwrap_or(prefs.left_handed);
    let edo = state.current_temperament().map(|t| t.divisions).unwrap_or(12);
    let note_names: Vec<String> = state
        .current_notation_system()
        .map(|ns| ns.note_names_for_key(edo, settings.key_natural_idx, settings.key_accidental_idx))
        .unwrap_or_default();
    let period = state.current_temperament()
        .map(|t| t.period.0 as f64 / t.period.1 as f64)
        .unwrap_or(2.0);
    drop(state);

    // In horizontal mode, compute fret count and per-fret spacing together so
    // the total board width stays constant regardless of EDO.
    let n_frets_h = {
        let n = (edo as f64 * 4_f64.ln() / period.ln()).round() as u32;
        n.clamp(8, 72)
    };
    let default_vs = settings.vertical_spacing as f64 / 1000.0;
    let vs_h = default_vs * 24.0 / n_frets_h as f64;

    let fret_style = FretboardStyle {
        display_markers_on_frets: settings.display_markers,
        fret_offset: settings.fret_offset,
        num_frets: if is_horizontal { n_frets_h } else { settings.num_frets },
        vertical_spacing: if is_horizontal { vs_h } else { default_vs },
        horizontal_spacing: settings.horizontal_spacing as f64 / 1000.0,
        horizontal: is_horizontal,
        edo,
        period,
        left_handed,
    };

    let note_names_ref: Option<&[String]> = if note_names.is_empty() { None } else { Some(&note_names) };
    let font_url = BRAVURA_FONT.to_string();

    let svg = match (maybe_item, maybe_tuning) {
        (Some(scale), Some(tuning)) => render_board(
            &prefs,
            &scale.name.clone(),
            settings.key as i32,
            &scale,
            tuning.skip_frets,
            &tuning,
            &fret_style,
            note_names_ref,
            &font_url,
            &playing_degs,
            &playing_stps,
            dark,
        ),
        _ => String::from(concat!(
            r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 30">"#,
            r#"<text x="10" y="18" font-size="10" fill="currentColor">"#,
            "No scale or tuning selected.",
            "</text></svg>",
        )),
    };

    rsx! {
        div {
            class: if is_horizontal { "fretboard-card horizontal" } else { "fretboard-card" },
            div {
                class: "fretboard-container",
                style: if is_horizontal {
                    "width: 100%;".to_string()
                } else {
                    format!("width: {}px; max-width: 100%;", settings.display_size)
                },
                dangerous_inner_html: "{svg}"
            }
        }
    }
}
