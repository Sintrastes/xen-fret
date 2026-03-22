use crate::components::{Modal, Select};
use crate::models::Tuning;
use crate::state::APP_STATE;
use dioxus::prelude::*;

#[component]
pub fn Tunings() -> Element {
    let mut show_modal = use_signal(|| false);
    let mut edit_key: Signal<Option<usize>> = use_signal(|| None); // flat index into state.tunings

    let mut form_name = use_signal(|| String::new());
    let mut form_instrument = use_signal(|| String::new());
    let mut form_tunings = use_signal(|| String::new());
    let mut form_skip = use_signal(|| String::from("0"));
    let mut form_root_octave = use_signal(|| String::from("2"));
    let mut selected_temp = use_signal(|| String::new());

    let temp_names: Vec<String> = APP_STATE
        .read()
        .temperaments
        .iter()
        .map(|t| t.name.clone())
        .collect();

    if selected_temp.read().is_empty() {
        if let Some(name) = temp_names.first() {
            selected_temp.set(name.clone());
        }
    }

    let tuning_groups: Vec<(String, Vec<(usize, String, String, String, u32)>)> = {
        let s = APP_STATE.read();
        let mut result = vec![];
        for t in &s.temperaments {
            let rows: Vec<_> = s.tunings.iter().enumerate()
                .filter(|(_, tu)| tu.temperament_name == t.name)
                .map(|(ui, tu)| {
                    let tuning_str = tu.string_tunings.iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    (ui, tu.instrument.clone(), tu.name.clone(), tuning_str, tu.skip_frets)
                })
                .collect();
            if !rows.is_empty() {
                result.push((t.name.clone(), rows));
            }
        }
        result
    };

    let open_add = move |_| {
        form_name.set(String::new());
        form_instrument.set(String::new());
        form_tunings.set(String::new());
        form_skip.set("0".into());
        form_root_octave.set("2".into());
        edit_key.set(None);
        show_modal.set(true);
    };

    let mut open_edit = move |ui: usize| {
        let s = APP_STATE.read();
        if let Some(tu) = s.tunings.get(ui) {
            form_name.set(tu.name.clone());
            form_instrument.set(tu.instrument.clone());
            form_tunings.set(
                tu.string_tunings.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "),
            );
            form_skip.set(tu.skip_frets.to_string());
            form_root_octave.set(tu.root_octave.to_string());
            selected_temp.set(tu.temperament_name.clone());
            drop(s);
            edit_key.set(Some(ui));
            show_modal.set(true);
        }
    };

    let save = move |_| {
        let name = form_name.read().trim().to_string();
        let instrument = form_instrument.read().trim().to_string();
        let string_tunings: Vec<i32> = form_tunings
            .read()
            .split(',')
            .filter_map(|s| s.trim().parse().ok())
            .collect();
        let skip_frets = form_skip.read().trim().parse().unwrap_or(0);
        let root_octave: i32 = form_root_octave.read().trim().parse().unwrap_or(2);
        let temp_name = selected_temp.read().clone();

        {
            let mut s = APP_STATE.write();
            if let Some(ui) = *edit_key.read() {
                if let Some(tu) = s.tunings.get_mut(ui) {
                    tu.name = name;
                    tu.instrument = instrument;
                    tu.string_tunings = string_tunings;
                    tu.skip_frets = skip_frets;
                    tu.root_octave = root_octave;
                }
            } else {
                s.tunings.push(Tuning {
                    temperament_name: temp_name,
                    name,
                    instrument,
                    string_tunings,
                    skip_frets,
                    fret_markers: vec![],
                    root_octave,
                });
            }
        }
        crate::storage::save(&*APP_STATE.read());
    };

    let mut delete = move |ui: usize| {
        {
            let mut s = APP_STATE.write();
            if ui < s.tunings.len() {
                s.tunings.remove(ui);
                s.selected_tuning_idx = 0;
            }
        }
        crate::storage::save(&*APP_STATE.read());
    };

    rsx! {
        div { class: "page",
            div { class: "page-header",
                div {
                    h1 { class: "page-title", "Tunings" }
                    p { class: "page-subtitle", "Define string tunings for each instrument and temperament." }
                }
                button { class: "btn btn-primary", onclick: open_add, "+ New Tuning" }
            }

            if tuning_groups.is_empty() {
                div { class: "empty-state",
                    span { class: "empty-icon", "♫" }
                    p { "No tunings defined yet." }
                    button { class: "btn btn-primary", onclick: open_add, "Add your first tuning" }
                }
            } else {
                div { class: "group-list",
                    for (temp_name, rows) in &tuning_groups {
                        div { class: "card group-card",
                            div { class: "group-header",
                                span { class: "badge badge-outline", "{temp_name}" }
                            }
                            table { class: "data-table",
                                thead {
                                    tr {
                                        th { "Instrument" }
                                        th { "Name" }
                                        th { "Strings" }
                                        th { "Skip" }
                                        th { class: "col-actions", "Actions" }
                                    }
                                }
                                tbody {
                                    for (ui, instrument, name, tuning_str, skip) in rows {
                                        tr {
                                            td { span { class: "item-name", "{instrument}" } }
                                            td { "{name}" }
                                            td { code { class: "mono", "{tuning_str}" } }
                                            td { "{skip}" }
                                            td { class: "col-actions",
                                                button {
                                                    class: "btn-icon",
                                                    title: "Edit",
                                                    onclick: { let ui = *ui; move |_| open_edit(ui) },
                                                    "✎"
                                                }
                                                button {
                                                    class: "btn-icon btn-icon-danger",
                                                    title: "Delete",
                                                    onclick: { let ui = *ui; move |_| delete(ui) },
                                                    "✕"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Modal {
                show: show_modal,
                title: if edit_key.read().is_some() { "Edit Tuning".to_string() } else { "New Tuning".to_string() },
                on_confirm: save,
                confirm_label: Some("Save".into()),

                div { class: "form-stack",
                    if edit_key.read().is_none() {
                        div { class: "form-group",
                            label { class: "form-label", "Temperament" }
                            Select {
                                value: selected_temp.read().clone(),
                                options: temp_names.iter().map(|name| (name.clone(), name.clone())).collect(),
                                on_change: move |val: String| selected_temp.set(val),
                            }
                        }
                    }
                    div { class: "form-row",
                        div { class: "form-group",
                            label { class: "form-label", "Instrument" }
                            input {
                                class: "form-input",
                                r#type: "text",
                                placeholder: "Guitar",
                                value: "{form_instrument}",
                                oninput: move |e| form_instrument.set(e.value())
                            }
                        }
                        div { class: "form-group",
                            label { class: "form-label", "Tuning Name" }
                            input {
                                class: "form-input",
                                r#type: "text",
                                placeholder: "Standard",
                                value: "{form_name}",
                                oninput: move |e| form_name.set(e.value())
                            }
                        }
                    }
                    div { class: "form-group",
                        label { class: "form-label", "String Tunings (comma-separated EDO steps)" }
                        input {
                            class: "form-input",
                            r#type: "text",
                            placeholder: "40, 45, 50, 55, 59, 64",
                            value: "{form_tunings}",
                            oninput: move |e| form_tunings.set(e.value())
                        }
                        p { class: "form-hint", "Enter pitch of each string in EDO steps from the unison, low to high." }
                    }
                    div { class: "form-row",
                        div { class: "form-group",
                            label { class: "form-label", "Skip Frets" }
                            input {
                                class: "form-input",
                                r#type: "number",
                                min: "0",
                                value: "{form_skip}",
                                oninput: move |e| form_skip.set(e.value())
                            }
                        }
                        div { class: "form-group",
                            label { class: "form-label", "Root Octave" }
                            input {
                                class: "form-input",
                                r#type: "number",
                                value: "{form_root_octave}",
                                oninput: move |e| form_root_octave.set(e.value())
                            }
                            p { class: "form-hint", "Octave of the lowest string (e.g. 2 for E2 guitar, 1 for bass)." }
                        }
                    }
                }
            }
        }
    }
}
