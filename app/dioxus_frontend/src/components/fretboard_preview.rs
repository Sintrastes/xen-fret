use dioxus::prelude::*;

use app_common::hit_test::{hit_test, HitTarget};
use crate::state::{DiagramMode, APP_STATE};
use fretboard_diagrams::{render_board_with_layout, FretboardStyle};
use xen_theory::scale::Scale;

// Ensure Bravura is bundled and served. asset!() registers the file with the
// dev server and gives a content-hashed URL in release builds.
static BRAVURA_FONT: Asset = asset!("/assets/Bravura.woff2");

#[component]
pub fn FretboardPreview(
    playing_degrees: ReadOnlySignal<Vec<usize>>,
    playing_steps: ReadOnlySignal<Vec<i32>>,
    horizontal_override: Option<bool>,
    on_note_click: Option<EventHandler<HitTarget>>,
) -> Element {
    let playing_degs = playing_degrees.read().clone();
    let playing_stps = playing_steps.read().clone();
    let state = APP_STATE.read();

    let prefs = state.preferences.clone();
    let settings = state.diagram_settings.fretboard_style.clone();
    let diagram_settings = state.diagram_settings.clone();

    let dark = state.effective_dark_mode();
    let is_horizontal = horizontal_override.unwrap_or(settings.horizontal);

    // Resolve the current item (scale or chord) as a Scale for rendering.
    // Both types have identical fields so the conversion is free.
    let maybe_item: Option<Scale> = match state.diagram_settings.mode {
        DiagramMode::Scale => state.current_scale().cloned(),
        DiagramMode::Chord => state.current_chord().map(|c| Scale {
            temperament_name: String::new(),
            name: c.name.clone(),
            intervals: c.intervals.clone(),
        }),
    };
    let maybe_tuning = state.current_tuning().cloned();
    // Resolve effective handedness: instrument override takes priority over global preference.
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
    drop(state);

    // In horizontal mode, compute fret count and per-fret spacing together so
    // the total board width stays constant regardless of EDO.
    let n_frets_h = {
        let n = (edo as f64 * 4_f64.ln() / period.ln()).round() as u32;
        n.clamp(8, 72)
    };
    let default_vs = settings.vertical_spacing;
    let vs_h = default_vs * 24.0 / n_frets_h as f64;

    let fret_style = FretboardStyle {
        title: settings.title,
        colors: prefs.diagram_colors(dark),
        scale_dots: settings.scale_dots,
        fret_markers: settings.fret_markers,
        display_string_names: settings.display_string_names,
        fret: settings.fret,
        fret_offset: settings.fret_offset,
        num_frets: if is_horizontal {
            n_frets_h
        } else {
            settings.num_frets
        },
        vertical_spacing: if is_horizontal { vs_h } else { default_vs },
        horizontal_spacing: settings.horizontal_spacing,
        horizontal: is_horizontal,
        left_handed,
    };

    let note_names_ref: Option<&[String]> = if note_names.is_empty() {
        None
    } else {
        Some(&note_names)
    };
    let font_url = BRAVURA_FONT.to_string();

    let (svg, layout) = match (maybe_item, maybe_tuning) {
        (Some(scale), Some(tuning)) => {
            let (svg, layout) = render_board_with_layout(
                diagram_settings.key as i32,
                &scale,
                tuning.skip_frets,
                &tuning,
                &fret_style,
                note_names_ref,
                &font_url,
                &playing_degs,
                &playing_stps,
            );
            (svg, Some(layout))
        }
        _ => (
            String::from(concat!(
                r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 30">"#,
                r#"<text x="10" y="18" font-size="10" fill="currentColor">"#,
                "No scale or tuning selected.",
                "</text></svg>",
            )),
            None,
        ),
    };

    rsx! {
        div {
            class: if is_horizontal { "fretboard-card horizontal" } else { "fretboard-card" },
            div {
                id: "fretboard-hit-area",
                class: "fretboard-container",
                style: if is_horizontal {
                    "width: 100%;".to_string()
                } else {
                    format!("width: {}px; max-width: 100%;", diagram_settings.display_size)
                },
                onpointerdown: move |evt: Event<PointerData>| {
                    let Some(layout) = layout.clone() else { return };
                    let Some(handler) = on_note_click else { return };
                    let (cx, cy) = (evt.data().client_coordinates().x, evt.data().client_coordinates().y);
                    spawn(async move {
                        // elementFromPoint confirms the press landed inside the fretboard
                        // (not on a dropdown overlay or other element on top). If it
                        // didn't, we send nothing and return — no state change, no
                        // interference with subsequent click events on other elements.
                        let js = format!(
                            "let el = document.getElementById('fretboard-hit-area'); \
                             if (el) {{ \
                               let t = document.elementFromPoint({cx}, {cy}); \
                               if (el.contains(t)) {{ \
                                 let r = el.getBoundingClientRect(); \
                                 dioxus.send([r.left, r.top, r.width, r.height]); \
                               }} \
                             }}"
                        );
                        let mut ev = document::eval(&js);
                        let Ok(rect) = ev.recv::<[f64; 4]>().await else { return };
                        let [left, top, w, h] = rect;
                        if let Some(hit) = hit_test(&layout, w, h, cx - left, cy - top) {
                            handler.call(hit);
                        }
                    });
                },
                dangerous_inner_html: "{svg}"
            }
        }
    }
}
