use crate::components::{Combobox, Modal, Select};
use crate::models::{default_instrument_types, default_string_count, suggest_tuning, Instrument};
use crate::state::APP_STATE;
use dioxus::prelude::*;

static BRAVURA_FONT: Asset = asset!("/assets/Bravura.woff2");

#[component]
pub fn InstrumentPicker(show: Signal<bool>) -> Element {
    let mut show_new_form = use_signal(|| false);
    let mut pending_delete: Signal<Option<usize>> = use_signal(|| None);
    let mut show_confirm = use_signal(|| false);
    let mut pending_edit: Signal<Option<usize>> = use_signal(|| None);
    let mut show_edit_form = use_signal(|| false);

    let instruments: Vec<(usize, String, String, String, u32)> = APP_STATE
        .read()
        .instruments
        .iter()
        .enumerate()
        .map(|(i, inst)| {
            (
                i,
                inst.name.clone(),
                inst.instrument_type.clone(),
                inst.temperament_name.clone(),
                inst.num_strings,
            )
        })
        .collect();
    let selected_idx = APP_STATE.read().selected_instrument_idx;
    let pending_delete_name: Option<String> = (*pending_delete.read()).and_then(|i| {
        instruments.get(i).map(|(_, name, ..)| name.clone())
    });

    rsx! {
        Modal {
            show,
            title: "Select Instrument".to_string(),

            div { class: "instrument-grid",
                for (idx, name, itype, temp_name, num_strings) in &instruments {
                    div {
                        class: if selected_idx == Some(*idx) { "instrument-card instrument-card--selected" } else { "instrument-card" },
                        onclick: {
                            let idx = *idx;
                            let inst_name = name.clone();
                            move |_| {
                                let mut s = APP_STATE.write();
                                s.selected_instrument_idx = Some(idx);
                                s.preferences.default_instrument = Some(inst_name.clone());
                                s.selected_tuning_idx = 0;
                                s.selected_scale_idx = 0;
                                s.selected_chord_idx = 0;
                                s.diagram_settings.key = 0;
                                s.diagram_settings.key_natural_idx = None;
                                s.diagram_settings.key_accidental_idx = None;
                                drop(s);
                                crate::storage::save(&*APP_STATE.read());
                                show.set(false);
                            }
                        },
                        button {
                            class: "instrument-card-edit",
                            title: "Edit instrument",
                            onclick: {
                                let idx = *idx;
                                move |e: MouseEvent| {
                                    e.stop_propagation();
                                    pending_edit.set(Some(idx));
                                    show_edit_form.set(true);
                                }
                            },
                            "✎"
                        }
                        button {
                            class: "instrument-card-delete",
                            title: "Delete instrument",
                            onclick: {
                                let idx = *idx;
                                move |e: MouseEvent| {
                                    e.stop_propagation();
                                    pending_delete.set(Some(idx));
                                    show_confirm.set(true);
                                }
                            },
                            "✕"
                        }
                        div { class: "instrument-card-name", "{name}" }
                        div { class: "instrument-card-meta",
                            span { class: "badge badge-outline", "{itype}" }
                            span { class: "instrument-card-strings", "{num_strings} strings" }
                        }
                        div { class: "instrument-card-temp",
                            span { class: "badge", "{temp_name}" }
                        }
                    }
                }

                // "Add new" card
                div {
                    class: "instrument-card instrument-card--add",
                    onclick: move |_| show_new_form.set(true),
                    span { class: "instrument-card-add-icon", "+" }
                    span { class: "instrument-card-add-label", "New Instrument" }
                }
            }
        }

        // Confirm-delete modal (unconditional — hooks must not be called conditionally)
        Modal {
            show: show_confirm,
            title: "Delete Instrument".to_string(),
            on_confirm: move |_| {
                if let Some(del_idx) = *pending_delete.read() {
                    let mut s = APP_STATE.write();
                    if del_idx < s.instruments.len() {
                        s.instruments.remove(del_idx);
                        match s.selected_instrument_idx {
                            Some(sel) if sel == del_idx => {
                                s.selected_instrument_idx = None;
                                s.preferences.default_instrument = None;
                            }
                            Some(sel) if sel > del_idx => {
                                s.selected_instrument_idx = Some(sel - 1);
                            }
                            _ => {}
                        }
                    }
                    drop(s);
                    crate::storage::save(&*APP_STATE.read());
                }
                pending_delete.set(None);
            },
            confirm_label: Some("Delete".into()),
            on_cancel: move |_| pending_delete.set(None),

            if let Some(ref name) = pending_delete_name {
                p { "Delete \"{name}\"? This cannot be undone." }
            }
        }

        NewInstrumentForm { show: show_new_form, picker_show: show }
        EditInstrumentForm { show: show_edit_form, edit_idx: pending_edit }
    }
}

#[component]
fn NewInstrumentForm(show: Signal<bool>, picker_show: Signal<bool>) -> Element {
    let mut form_name = use_signal(|| String::new());
    let mut form_type = use_signal(|| "Guitar".to_string());
    let mut form_temp = use_signal(|| String::new());
    let mut form_strings = use_signal(|| 6u32);
    let mut form_frets = use_signal(|| 24u32);
    // None = use global default, Some(false) = right-handed, Some(true) = left-handed.
    let mut form_left_handed: Signal<Option<bool>> = use_signal(|| None);

    // Initialize default temperament name on first render
    let temp_names: Vec<(String, String)> = APP_STATE
        .read()
        .temperaments
        .iter()
        .map(|t| (t.name.clone(), t.name.clone()))
        .collect();
    if form_temp.read().is_empty() && !temp_names.is_empty() {
        let state = APP_STATE.read();
        let pref = &state.preferences.default_temperament;
        let default = temp_names.iter().find(|(k, _)| k == pref)
            .or_else(|| temp_names.iter().find(|(k, _)| k == "12-TET"))
            .or_else(|| temp_names.first())
            .map(|(k, _)| k.clone())
            .unwrap_or_default();
        drop(state);
        form_temp.set(default);
    }

    let instrument_types: Vec<(String, String)> = default_instrument_types()
        .iter()
        .map(|t| (t.to_string(), t.to_string()))
        .collect();

    // Compute suggested tuning note names reactively
    let tuning_note_names: Vec<String> = {
        let state = APP_STATE.read();
        let steps = suggest_tuning(
            &state.tunings,
            &state.temperaments,
            &form_temp.read(),
            &form_type.read(),
            *form_strings.read(),
        );
        let temp = state.temperaments.iter().find(|t| t.name == *form_temp.read());
        let ns = temp.and_then(|t| {
            state.notation_systems.iter().find(|ns| ns.temperament_name == t.name)
        });
        match ns {
            Some(ns) => {
                let edo = temp.unwrap().divisions;
                let names = ns.note_names(edo);
                steps.iter().map(|&s| {
                    let degree = s.rem_euclid(edo as i32) as usize;
                    names.get(degree).cloned().unwrap_or_else(|| s.to_string())
                }).collect()
            }
            None => steps.iter().map(|s| s.to_string()).collect(),
        }
    };

    let save = move |_| {
        let name = form_name.read().trim().to_string();
        let name = if name.is_empty() {
            format!("{} ({})", form_type.read(), form_temp.read())
        } else {
            name
        };
        let instrument = Instrument {
            name,
            instrument_type: form_type.read().clone(),
            temperament_name: form_temp.read().clone(),
            num_strings: *form_strings.read(),
            num_frets: *form_frets.read(),
            fret_markers: if *form_frets.read() >= 24 {
                crate::models::guitar_markers()
            } else {
                vec![]
            },
            left_handed: *form_left_handed.read(),
        };
        let mut s = APP_STATE.write();
        s.instruments.push(instrument);
        let new_idx = s.instruments.len() - 1;
        s.selected_instrument_idx = Some(new_idx);
        s.preferences.default_instrument = s.instruments.get(new_idx).map(|i| i.name.clone());
        s.selected_tuning_idx = 0;
        s.selected_scale_idx = 0;
        s.selected_chord_idx = 0;
        s.diagram_settings.key = 0;
        s.diagram_settings.key_natural_idx = None;
        s.diagram_settings.key_accidental_idx = None;
        drop(s);
        crate::storage::save(&*APP_STATE.read());

        // Reset form
        form_name.set(String::new());
        form_type.set("Guitar".to_string());
        form_strings.set(6);
        form_frets.set(24);
        form_left_handed.set(None);

        // Close both modals
        show.set(false);
        picker_show.set(false);
    };

    let font_url = BRAVURA_FONT.to_string();
    let font_face_css = format!(
        "@font-face{{font-family:'Bravura';src:url('{}') format('woff2')}}",
        font_url
    );

    rsx! {
        style { dangerous_inner_html: "{font_face_css}" }
        Modal {
            show,
            title: "New Instrument".to_string(),
            on_confirm: save,
            confirm_label: Some("Add".into()),

            div { class: "form-stack",
                div { class: "form-group",
                    label { class: "form-label", "Name (optional)" }
                    input {
                        class: "form-input",
                        r#type: "text",
                        placeholder: "My Guitar",
                        value: "{form_name}",
                        oninput: move |e| form_name.set(e.value()),
                    }
                }

                div { class: "form-row",
                    div { class: "form-group",
                        label { class: "form-label", "Instrument Type" }
                        Select {
                            value: form_type.read().clone(),
                            options: instrument_types,
                            on_change: move |val: String| {
                                form_strings.set(default_string_count(&val));
                                form_type.set(val);
                            },
                        }
                    }
                    div { class: "form-group",
                        label { class: "form-label", "Temperament" }
                        Combobox {
                            value: form_temp.read().clone(),
                            options: temp_names,
                            on_change: move |val: String| form_temp.set(val),
                        }
                    }
                }

                div { class: "form-row",
                    div { class: "form-group",
                        label { class: "form-label", "Number of Strings" }
                        input {
                            class: "form-input",
                            r#type: "number",
                            min: "1",
                            max: "24",
                            value: "{form_strings}",
                            oninput: move |e| {
                                if let Ok(v) = e.value().parse::<u32>() {
                                    form_strings.set(v);
                                }
                            },
                        }
                    }
                    div { class: "form-group",
                        label { class: "form-label", "Number of Frets" }
                        input {
                            class: "form-input",
                            r#type: "number",
                            min: "1",
                            max: "48",
                            value: "{form_frets}",
                            oninput: move |e| {
                                if let Ok(v) = e.value().parse::<u32>() {
                                    form_frets.set(v);
                                }
                            },
                        }
                    }
                }

                div { class: "form-group",
                    label { class: "form-label", "Handedness" }
                    div { class: "radio-group",
                        label { class: "radio-option",
                            input {
                                r#type: "radio", name: "new-handedness",
                                checked: form_left_handed.read().is_none(),
                                onchange: move |_| form_left_handed.set(None)
                            }
                            "Use default"
                        }
                        label { class: "radio-option",
                            input {
                                r#type: "radio", name: "new-handedness",
                                checked: *form_left_handed.read() == Some(false),
                                onchange: move |_| form_left_handed.set(Some(false))
                            }
                            "Right-handed"
                        }
                        label { class: "radio-option",
                            input {
                                r#type: "radio", name: "new-handedness",
                                checked: *form_left_handed.read() == Some(true),
                                onchange: move |_| form_left_handed.set(Some(true))
                            }
                            "Left-handed"
                        }
                    }
                }

                div { class: "form-group",
                    label { class: "form-label", "Suggested Tuning" }
                    div { class: "form-hint-box tuning-name-row",
                        for (i, name) in tuning_note_names.iter().enumerate() {
                            if i > 0 { span { class: "tuning-name-sep", "," } }
                            span { class: "tuning-name-item", "{name}" }
                        }
                    }
                    p { class: "form-hint", "Based on instrument type, temperament, and string count. You can customize tunings on the Tunings page." }
                }
            }
        }
    }
}

#[component]
fn EditInstrumentForm(show: Signal<bool>, edit_idx: Signal<Option<usize>>) -> Element {
    let mut form_name = use_signal(|| String::new());
    let mut form_type = use_signal(|| String::new());
    let mut form_temp = use_signal(|| String::new());
    let mut form_strings = use_signal(|| 6u32);
    let mut form_frets = use_signal(|| 24u32);
    let mut form_left_handed: Signal<Option<bool>> = use_signal(|| None);
    // Tracks which index we've loaded into the form — re-initializes when it changes.
    let mut initialized_for: Signal<Option<usize>> = use_signal(|| None);

    let temp_names: Vec<(String, String)> = APP_STATE
        .read()
        .temperaments
        .iter()
        .map(|t| (t.name.clone(), t.name.clone()))
        .collect();

    let instrument_types: Vec<(String, String)> = default_instrument_types()
        .iter()
        .map(|t| (t.to_string(), t.to_string()))
        .collect();

    // Re-initialize form fields when a different instrument is opened for editing.
    let current_idx = *edit_idx.read();
    if current_idx != *initialized_for.read() {
        if let Some(idx) = current_idx {
            let state = APP_STATE.read();
            if let Some(inst) = state.instruments.get(idx) {
                form_name.set(inst.name.clone());
                form_type.set(inst.instrument_type.clone());
                form_temp.set(inst.temperament_name.clone());
                form_strings.set(inst.num_strings);
                form_frets.set(inst.num_frets);
                form_left_handed.set(inst.left_handed);
            }
        }
        initialized_for.set(current_idx);
    }

    // Suggested tuning preview based on current form values.
    let tuning_note_names: Vec<String> = {
        let state = APP_STATE.read();
        let steps = suggest_tuning(
            &state.tunings,
            &state.temperaments,
            &form_temp.read(),
            &form_type.read(),
            *form_strings.read(),
        );
        let temp = state.temperaments.iter().find(|t| t.name == *form_temp.read());
        let ns = temp.and_then(|t| {
            state.notation_systems.iter().find(|ns| ns.temperament_name == t.name)
        });
        match ns {
            Some(ns) => {
                let edo = temp.unwrap().divisions;
                let names = ns.note_names(edo);
                steps.iter().map(|&s| {
                    let degree = s.rem_euclid(edo as i32) as usize;
                    names.get(degree).cloned().unwrap_or_else(|| s.to_string())
                }).collect()
            }
            None => steps.iter().map(|s| s.to_string()).collect(),
        }
    };

    let save = move |_| {
        if let Some(idx) = *edit_idx.read() {
            let name = form_name.read().trim().to_string();
            let name = if name.is_empty() {
                format!("{} ({})", form_type.read(), form_temp.read())
            } else {
                name
            };
            let mut s = APP_STATE.write();
            if let Some(inst) = s.instruments.get_mut(idx) {
                inst.name = name.clone();
                inst.instrument_type = form_type.read().clone();
                inst.temperament_name = form_temp.read().clone();
                inst.num_strings = *form_strings.read();
                inst.num_frets = *form_frets.read();
                inst.left_handed = *form_left_handed.read();
                if inst.fret_markers.is_empty() && inst.num_frets >= 24 {
                    inst.fret_markers = crate::models::guitar_markers();
                }
            }
            // Keep default_instrument preference in sync if the name changed.
            if s.preferences.default_instrument.as_deref()
                == s.instruments.get(idx).map(|i| i.name.as_str())
            {
                s.preferences.default_instrument = Some(name);
            }
            drop(s);
            crate::storage::save(&*APP_STATE.read());
        }
        show.set(false);
    };

    rsx! {
        Modal {
            show,
            title: "Edit Instrument".to_string(),
            on_confirm: save,
            confirm_label: Some("Save".into()),

            div { class: "form-stack",
                div { class: "form-group",
                    label { class: "form-label", "Name" }
                    input {
                        class: "form-input",
                        r#type: "text",
                        placeholder: "My Guitar",
                        value: "{form_name}",
                        oninput: move |e| form_name.set(e.value()),
                    }
                }

                div { class: "form-row",
                    div { class: "form-group",
                        label { class: "form-label", "Instrument Type" }
                        Select {
                            value: form_type.read().clone(),
                            options: instrument_types,
                            on_change: move |val: String| {
                                form_strings.set(default_string_count(&val));
                                form_type.set(val);
                            },
                        }
                    }
                    div { class: "form-group",
                        label { class: "form-label", "Temperament" }
                        Combobox {
                            value: form_temp.read().clone(),
                            options: temp_names,
                            on_change: move |val: String| form_temp.set(val),
                        }
                    }
                }

                div { class: "form-row",
                    div { class: "form-group",
                        label { class: "form-label", "Number of Strings" }
                        input {
                            class: "form-input",
                            r#type: "number",
                            min: "1",
                            max: "24",
                            value: "{form_strings}",
                            oninput: move |e| {
                                if let Ok(v) = e.value().parse::<u32>() {
                                    form_strings.set(v);
                                }
                            },
                        }
                    }
                    div { class: "form-group",
                        label { class: "form-label", "Number of Frets" }
                        input {
                            class: "form-input",
                            r#type: "number",
                            min: "1",
                            max: "48",
                            value: "{form_frets}",
                            oninput: move |e| {
                                if let Ok(v) = e.value().parse::<u32>() {
                                    form_frets.set(v);
                                }
                            },
                        }
                    }
                }

                div { class: "form-group",
                    label { class: "form-label", "Handedness" }
                    div { class: "radio-group",
                        label { class: "radio-option",
                            input {
                                r#type: "radio", name: "edit-handedness",
                                checked: form_left_handed.read().is_none(),
                                onchange: move |_| form_left_handed.set(None)
                            }
                            "Use default"
                        }
                        label { class: "radio-option",
                            input {
                                r#type: "radio", name: "edit-handedness",
                                checked: *form_left_handed.read() == Some(false),
                                onchange: move |_| form_left_handed.set(Some(false))
                            }
                            "Right-handed"
                        }
                        label { class: "radio-option",
                            input {
                                r#type: "radio", name: "edit-handedness",
                                checked: *form_left_handed.read() == Some(true),
                                onchange: move |_| form_left_handed.set(Some(true))
                            }
                            "Left-handed"
                        }
                    }
                }

                div { class: "form-group",
                    label { class: "form-label", "Suggested Tuning" }
                    div { class: "form-hint-box tuning-name-row",
                        for (i, name) in tuning_note_names.iter().enumerate() {
                            if i > 0 { span { class: "tuning-name-sep", "," } }
                            span { class: "tuning-name-item", "{name}" }
                        }
                    }
                    p { class: "form-hint", "Based on instrument type, temperament, and string count. You can customize tunings on the Tunings page." }
                }
            }
        }
    }
}
