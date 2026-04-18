use crate::components::{Modal, Select};
use xen_theory::chord::Chord;
use crate::state::APP_STATE;
use dioxus::prelude::*;

#[component]
pub fn Chords() -> Element {
    let mut show_modal = use_signal(|| false);
    let mut edit_key: Signal<Option<usize>> = use_signal(|| None); // flat index into state.chords

    let mut form_name = use_signal(|| String::new());
    let mut form_intervals = use_signal(|| String::new());
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

    let chord_groups: Vec<(String, Vec<(usize, String, String)>)> = {
        let s = APP_STATE.read();
        let mut result = vec![];
        for t in &s.temperaments {
            let rows: Vec<_> = s.chords.iter().enumerate()
                .filter(|(_, ch)| ch.temperament_name == t.name)
                .map(|(ci, ch)| {
                    let ivs = ch.intervals.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", ");
                    (ci, ch.name.clone(), ivs)
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
        form_intervals.set(String::new());
        edit_key.set(None);
        show_modal.set(true);
    };

    let mut open_edit = move |ci: usize| {
        let s = APP_STATE.read();
        if let Some(ch) = s.chords.get(ci) {
            form_name.set(ch.name.clone());
            form_intervals.set(
                ch.intervals.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "),
            );
            selected_temp.set(ch.temperament_name.clone());
            drop(s);
            edit_key.set(Some(ci));
            show_modal.set(true);
        }
    };

    let save = move |_| {
        let name = form_name.read().trim().to_string();
        let intervals: Vec<i32> = form_intervals
            .read()
            .split(',')
            .filter_map(|s| s.trim().parse().ok())
            .collect();
        let temp_name = selected_temp.read().clone();

        {
            let mut s = APP_STATE.write();
            if let Some(ci) = *edit_key.read() {
                s.update_chord(ci, name, intervals);
            } else {
                s.add_chord(Chord { temperament_name: temp_name, name, intervals });
            }
        }
        app_common::storage::save(&*APP_STATE.read());
    };

    let delete = move |ci: usize| {
        APP_STATE.write().delete_chord(ci);
        app_common::storage::save(&*APP_STATE.read());
    };

    rsx! {
        div { class: "page",
            div { class: "page-header",
                div {
                    h1 { class: "page-title", "Chords" }
                    p { class: "page-subtitle", "Define chords as interval stacks in EDO steps." }
                }
                button { class: "btn btn-primary", onclick: open_add, "+ New Chord" }
            }

            if chord_groups.is_empty() {
                div { class: "empty-state",
                    span { class: "empty-icon", "♬" }
                    p { "No chords defined yet." }
                    button { class: "btn btn-primary", onclick: open_add, "Add your first chord" }
                }
            } else {
                div { class: "group-list",
                    for (temp_name, rows) in &chord_groups {
                        div { class: "card group-card",
                            div { class: "group-header",
                                span { class: "badge badge-outline", "{temp_name}" }
                            }
                            table { class: "data-table",
                                thead {
                                    tr {
                                        th { "Name" }
                                        th { "Intervals" }
                                        th { class: "col-actions", "Actions" }
                                    }
                                }
                                tbody {
                                    for (ci, name, ivs) in rows {
                                        tr {
                                            td { span { class: "item-name", "{name}" } }
                                            td { code { class: "mono", "{ivs}" } }
                                            td { class: "col-actions",
                                                button {
                                                    class: "btn-icon",
                                                    onclick: { let ci = *ci; move |_| open_edit(ci) },
                                                    "✎"
                                                }
                                                button {
                                                    class: "btn-icon btn-icon-danger",
                                                    onclick: { let ci = *ci; move |_| delete(ci) },
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
                title: if edit_key.read().is_some() { "Edit Chord".to_string() } else { "New Chord".to_string() },
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
                    div { class: "form-group",
                        label { class: "form-label", "Chord Name" }
                        input {
                            class: "form-input",
                            r#type: "text",
                            placeholder: "Major",
                            value: "{form_name}",
                            oninput: move |e| form_name.set(e.value())
                        }
                    }
                    div { class: "form-group",
                        label { class: "form-label", "Intervals (comma-separated EDO steps)" }
                        input {
                            class: "form-input",
                            r#type: "text",
                            placeholder: "4, 3, 5",
                            value: "{form_intervals}",
                            oninput: move |e| form_intervals.set(e.value())
                        }
                        p { class: "form-hint",
                            "Intervals are measured from root upward. E.g. Major = 4, 3, 5 in 12-EDO."
                        }
                    }
                }
            }
        }
    }
}
