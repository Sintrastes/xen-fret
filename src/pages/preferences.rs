use crate::components::{ColorPicker, Combobox, Select};
use crate::models::{FretStyle, PitchDetectorKind, ThemeMode};
use crate::state::APP_STATE;
use dioxus::prelude::*;

#[component]
pub fn Preferences() -> Element {
    let prefs = APP_STATE.read().preferences.clone();
    let settings = APP_STATE.read().diagram_settings.clone();
    let temp_names: Vec<String> = APP_STATE
        .read()
        .temperaments
        .iter()
        .map(|t| t.name.clone())
        .collect();
    rsx! {
        div { class: "page page-preferences",
            div { class: "page-header",
                div {
                    h1 { class: "page-title", "Preferences" }
                    p { class: "page-subtitle", "Customize diagram appearance and app defaults." }
                }
            }

            div { class: "prefs-grid",

                // Appearance card
                div { class: "card prefs-card",
                    h2 { class: "card-heading", "Appearance" }

                    div { class: "form-group",
                        label { class: "form-label", "Theme" }
                        div { class: "radio-group",
                            label { class: "radio-option",
                                input {
                                    r#type: "radio",
                                    name: "theme-mode",
                                    checked: matches!(prefs.theme_mode, ThemeMode::System),
                                    onchange: move |_| {
                                        APP_STATE.write().preferences.theme_mode = ThemeMode::System;
                                    }
                                }
                                "System (auto-detect)"
                            }
                            label { class: "radio-option",
                                input {
                                    r#type: "radio",
                                    name: "theme-mode",
                                    checked: matches!(prefs.theme_mode, ThemeMode::Light),
                                    onchange: move |_| {
                                        APP_STATE.write().preferences.theme_mode = ThemeMode::Light;
                                    }
                                }
                                "Light"
                            }
                            label { class: "radio-option",
                                input {
                                    r#type: "radio",
                                    name: "theme-mode",
                                    checked: matches!(prefs.theme_mode, ThemeMode::Dark),
                                    onchange: move |_| {
                                        APP_STATE.write().preferences.theme_mode = ThemeMode::Dark;
                                    }
                                }
                                "Dark"
                            }
                        }
                    }
                }

                // Diagram colors card
                div { class: "card prefs-card prefs-card-wide",
                    h2 { class: "card-heading", "Diagram Colors" }

                    div { class: "color-theme-columns",
                        // Light mode column
                        div {
                            h3 { class: "prefs-subheading", "Light Mode" }

                            div { class: "color-field",
                                label { class: "color-field-label", "Root Note" }
                                ColorPicker {
                                    value: prefs.root_note_color,
                                    on_change: move |c| { APP_STATE.write().preferences.root_note_color = c; }
                                }
                            }
                            div { class: "color-field",
                                label { class: "color-field-label", "Scale Notes" }
                                ColorPicker {
                                    value: prefs.scale_note_color,
                                    on_change: move |c| { APP_STATE.write().preferences.scale_note_color = c; }
                                }
                            }
                            div { class: "color-field",
                                label { class: "color-field-label", "Fretboard" }
                                ColorPicker {
                                    value: prefs.fretboard_color,
                                    on_change: move |c| { APP_STATE.write().preferences.fretboard_color = c; }
                                }
                            }
                            div { class: "color-field",
                                label { class: "color-field-label", "Labels" }
                                ColorPicker {
                                    value: prefs.label_color,
                                    on_change: move |c| { APP_STATE.write().preferences.label_color = c; }
                                }
                            }
                        }

                        // Dark mode column
                        div {
                            h3 { class: "prefs-subheading", "Dark Mode" }

                            div { class: "color-field",
                                label { class: "color-field-label", "Root Note" }
                                ColorPicker {
                                    value: prefs.dark_root_note_color,
                                    on_change: move |c| { APP_STATE.write().preferences.dark_root_note_color = c; }
                                }
                            }
                            div { class: "color-field",
                                label { class: "color-field-label", "Scale Notes" }
                                ColorPicker {
                                    value: prefs.dark_scale_note_color,
                                    on_change: move |c| { APP_STATE.write().preferences.dark_scale_note_color = c; }
                                }
                            }
                            div { class: "color-field",
                                label { class: "color-field-label", "Fretboard" }
                                ColorPicker {
                                    value: prefs.dark_fretboard_color,
                                    on_change: move |c| { APP_STATE.write().preferences.dark_fretboard_color = c; }
                                }
                            }
                            div { class: "color-field",
                                label { class: "color-field-label", "Labels" }
                                ColorPicker {
                                    value: prefs.dark_label_color,
                                    on_change: move |c| { APP_STATE.write().preferences.dark_label_color = c; }
                                }
                            }
                        }
                    }
                }

                // Diagram style card
                div { class: "card prefs-card",
                    h2 { class: "card-heading", "Diagram Style" }

                    div { class: "form-row",
                        div { class: "form-group",
                            label { class: "form-label", "Note Name Size" }
                            input {
                                class: "form-input",
                                r#type: "number",
                                min: "8",
                                max: "32",
                                value: "{prefs.note_name_size}",
                                oninput: move |e| {
                                    if let Ok(v) = e.value().parse::<u32>() {
                                        APP_STATE.write().preferences.note_name_size = v;
                                    }
                                }
                            }
                        }
                        div { class: "form-group",
                            label { class: "form-label", "Dot Size" }
                            input {
                                class: "form-input",
                                r#type: "number",
                                min: "0.1",
                                max: "2.0",
                                step: "0.1",
                                value: "{prefs.dot_size}",
                                oninput: move |e| {
                                    if let Ok(v) = e.value().parse::<f64>() {
                                        APP_STATE.write().preferences.dot_size = v;
                                    }
                                }
                            }
                        }
                    }

                    div { class: "form-group",
                        label { class: "form-label", "Fret Style" }
                        div { class: "radio-group",
                            label { class: "radio-option",
                                input {
                                    r#type: "radio",
                                    name: "fret-style",
                                    checked: matches!(prefs.fret_style, FretStyle::Solid),
                                    onchange: move |_| {
                                        APP_STATE.write().preferences.fret_style = FretStyle::Solid;
                                    }
                                }
                                "Solid"
                            }
                            label { class: "radio-option",
                                input {
                                    r#type: "radio",
                                    name: "fret-style",
                                    checked: matches!(prefs.fret_style, FretStyle::Dashed),
                                    onchange: move |_| {
                                        APP_STATE.write().preferences.fret_style = FretStyle::Dashed;
                                    }
                                }
                                "Dashed"
                            }
                        }
                    }

                    div { class: "form-group",
                        label { class: "form-label", "Fret Thickness" }
                        input {
                            class: "form-input",
                            r#type: "number",
                            min: "0.5",
                            max: "5.0",
                            step: "0.25",
                            value: "{prefs.fret_thickness}",
                            oninput: move |e| {
                                if let Ok(v) = e.value().parse::<f64>() {
                                    APP_STATE.write().preferences.fret_thickness = v;
                                }
                            }
                        }
                    }
                }

                // Mic detection card
                div { class: "card prefs-card",
                    h2 { class: "card-heading", "Mic Detection" }

                    div { class: "form-group",
                        label { class: "form-label", "Pitch Detector" }
                        div { class: "radio-group",
                            label { class: "radio-option",
                                input {
                                    r#type: "radio",
                                    name: "pitch-detector",
                                    checked: matches!(prefs.pitch_detector, PitchDetectorKind::Yin),
                                    onchange: move |_| {
                                        APP_STATE.write().preferences.pitch_detector = PitchDetectorKind::Yin;
                                    }
                                }
                                "Monophonic (YIN)"
                            }
                            label { class: "radio-option",
                                input {
                                    r#type: "radio",
                                    name: "pitch-detector",
                                    checked: matches!(prefs.pitch_detector, PitchDetectorKind::IterF0),
                                    onchange: move |_| {
                                        APP_STATE.write().preferences.pitch_detector = PitchDetectorKind::IterF0;
                                    }
                                }
                                "Polyphonic (IterF0)"
                            }
                        }
                    }
                }

                // Defaults card
                div { class: "card prefs-card",
                    h2 { class: "card-heading", "Defaults" }

                    div { class: "form-group",
                        label { class: "form-label", "Default Temperament" }
                        Combobox {
                            value: prefs.default_temperament.clone(),
                            options: temp_names.iter().map(|n| (n.clone(), n.clone())).collect(),
                            on_change: move |val: String| {
                                APP_STATE.write().preferences.default_temperament = val;
                            },
                        }
                    }

                    div { class: "form-group",
                        label { class: "form-label", "Handedness" }
                        div { class: "radio-group",
                            label { class: "radio-option",
                                input {
                                    r#type: "radio",
                                    name: "handedness",
                                    checked: !prefs.left_handed,
                                    onchange: move |_| {
                                        APP_STATE.write().preferences.left_handed = false;
                                    }
                                }
                                "Right-handed"
                            }
                            label { class: "radio-option",
                                input {
                                    r#type: "radio",
                                    name: "handedness",
                                    checked: prefs.left_handed,
                                    onchange: move |_| {
                                        APP_STATE.write().preferences.left_handed = true;
                                    }
                                }
                                "Left-handed (mirror diagram)"
                            }
                        }
                        p { class: "form-hint", "Can be overridden per instrument." }
                    }
                }

                // Diagram options card
                div { class: "card prefs-card",
                    h2 { class: "card-heading", "Diagram Options" }

                    div { class: "form-row",
                        div { class: "form-group",
                            label { class: "form-label", "Size (px)" }
                            input {
                                class: "form-input",
                                r#type: "number",
                                min: "100",
                                max: "800",
                                value: "{settings.display_size}",
                                onchange: move |e| {
                                    if let Ok(v) = e.value().parse::<u32>() {
                                        APP_STATE.write().diagram_settings.display_size = v;
                                    }
                                }
                            }
                        }
                        div { class: "form-group",
                            label { class: "form-label", "Num Frets" }
                            input {
                                class: "form-input",
                                r#type: "number",
                                min: "1",
                                max: "24",
                                value: "{settings.num_frets}",
                                onchange: move |e| {
                                    if let Ok(v) = e.value().parse::<u32>() {
                                        APP_STATE.write().diagram_settings.num_frets = v;
                                    }
                                }
                            }
                        }
                    }

                    div { class: "form-row",
                        div { class: "form-group",
                            label { class: "form-label", "Vertical Spacing" }
                            input {
                                class: "form-input",
                                r#type: "number",
                                min: "50",
                                max: "500",
                                value: "{settings.vertical_spacing}",
                                onchange: move |e| {
                                    if let Ok(v) = e.value().parse::<u32>() {
                                        APP_STATE.write().diagram_settings.vertical_spacing = v;
                                    }
                                }
                            }
                        }
                        div { class: "form-group",
                            label { class: "form-label", "Horiz. Spacing" }
                            input {
                                class: "form-input",
                                r#type: "number",
                                min: "50",
                                max: "500",
                                value: "{settings.horizontal_spacing}",
                                onchange: move |e| {
                                    if let Ok(v) = e.value().parse::<u32>() {
                                        APP_STATE.write().diagram_settings.horizontal_spacing = v;
                                    }
                                }
                            }
                        }
                    }

                    div { class: "form-group form-group-inline",
                        label { class: "form-label", "Display fret markers" }
                        label { class: "toggle",
                            input {
                                r#type: "checkbox",
                                checked: settings.display_markers,
                                onchange: move |e| {
                                    APP_STATE.write().diagram_settings.display_markers = e.checked();
                                }
                            }
                            span { class: "toggle-slider" }
                        }
                    }
                }

                // Data management card
                div { class: "card prefs-card",
                    h2 { class: "card-heading", "Data" }
                    p { class: "card-description",
                        "Export or import your temperaments, scales, and chords as JSON."
                    }
                    div { class: "btn-group",
                        button {
                            class: "btn btn-outline",
                            onclick: move |_| { /* TODO: export */ },
                            "⬇ Export JSON"
                        }
                        button {
                            class: "btn btn-outline",
                            onclick: move |_| { /* TODO: import */ },
                            "⬆ Import JSON"
                        }
                        button {
                            class: "btn btn-ghost btn-danger",
                            onclick: move |_| { /* TODO: reset */ },
                            "↺ Reset to Defaults"
                        }
                    }
                }
            }
        }
    }
}

