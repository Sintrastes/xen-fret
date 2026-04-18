use crate::components::{Modal, Select};
use xen_theory::scale::Scale;
use crate::state::APP_STATE;
use dioxus::prelude::*;

#[component]
pub fn Scales() -> Element {
    let mut show_modal = use_signal(|| false);
    let mut edit_key: Signal<Option<usize>> = use_signal(|| None); // flat index into state.scales

    let mut form_name = use_signal(|| String::new());
    let mut form_intervals = use_signal(|| String::new());
    let mut selected_temp = use_signal(|| String::new());

    let temp_names: Vec<(String, u32)> = APP_STATE
        .read()
        .temperaments
        .iter()
        .map(|t| (t.name.clone(), t.divisions))
        .collect();

    if selected_temp.read().is_empty() {
        if let Some((name, _)) = temp_names.first() {
            selected_temp.set(name.clone());
        }
    }

    let scale_groups: Vec<(String, u32, Vec<(usize, String, String, i32)>)> = {
        let s = APP_STATE.read();
        let mut result = vec![];
        for t in &s.temperaments {
            let rows: Vec<_> = s.scales.iter().enumerate()
                .filter(|(_, sc)| sc.temperament_name == t.name)
                .map(|(si, sc)| {
                    let ivs = sc.intervals.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", ");
                    let total: i32 = sc.intervals.iter().sum();
                    (si, sc.name.clone(), ivs, total)
                })
                .collect();
            if !rows.is_empty() {
                result.push((t.name.clone(), t.divisions, rows));
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

    let mut open_edit = move |si: usize| {
        let s = APP_STATE.read();
        if let Some(sc) = s.scales.get(si) {
            form_name.set(sc.name.clone());
            form_intervals.set(
                sc.intervals.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "),
            );
            selected_temp.set(sc.temperament_name.clone());
            drop(s);
            edit_key.set(Some(si));
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
            if let Some(si) = *edit_key.read() {
                s.update_scale(si, name, intervals);
            } else {
                s.add_scale(Scale { temperament_name: temp_name, name, intervals });
            }
        }
        app_common::storage::save(&*APP_STATE.read());
    };

    let delete = move |si: usize| {
        APP_STATE.write().delete_scale(si);
        app_common::storage::save(&*APP_STATE.read());
    };

    rsx! {
        div { class: "page",
            div { class: "page-header",
                div {
                    h1 { class: "page-title", "Scales" }
                    p { class: "page-subtitle", "Define scales as sequences of intervals in EDO steps." }
                }
                button { class: "btn btn-primary", onclick: open_add, "+ New Scale" }
            }

            if scale_groups.is_empty() {
                div { class: "empty-state",
                    span { class: "empty-icon", "𝄞" }
                    p { "No scales defined yet." }
                    button { class: "btn btn-primary", onclick: open_add, "Add your first scale" }
                }
            } else {
                div { class: "group-list",
                    for (temp_name, divisions, rows) in &scale_groups {
                        div { class: "card group-card",
                            div { class: "group-header",
                                span { class: "badge badge-outline", "{temp_name}" }
                                span { class: "group-meta", "{divisions}-EDO" }
                            }
                            table { class: "data-table",
                                thead {
                                    tr {
                                        th { "Name" }
                                        th { "Intervals" }
                                        th { "Period Sum" }
                                        th { class: "col-actions", "Actions" }
                                    }
                                }
                                tbody {
                                    for (si, name, ivs, total) in rows {
                                        tr {
                                            td { span { class: "item-name", "{name}" } }
                                            td { code { class: "mono", "{ivs}" } }
                                            td {
                                                span {
                                                    class: if total == &(*divisions as i32) { "badge badge-success" } else { "badge badge-warn" },
                                                    "{total}"
                                                }
                                            }
                                            td { class: "col-actions",
                                                button {
                                                    class: "btn-icon",
                                                    onclick: { let si = *si; move |_| open_edit(si) },
                                                    "✎"
                                                }
                                                button {
                                                    class: "btn-icon btn-icon-danger",
                                                    onclick: { let si = *si; move |_| delete(si) },
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
                title: if edit_key.read().is_some() { "Edit Scale".to_string() } else { "New Scale".to_string() },
                on_confirm: save,
                confirm_label: Some("Save".into()),

                div { class: "form-stack",
                    if edit_key.read().is_none() {
                        div { class: "form-group",
                            label { class: "form-label", "Temperament" }
                            Select {
                                value: selected_temp.read().clone(),
                                options: temp_names.iter().map(|(name, _)| (name.clone(), name.clone())).collect(),
                                on_change: move |val: String| selected_temp.set(val),
                            }
                        }
                    }
                    div { class: "form-group",
                        label { class: "form-label", "Scale Name" }
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
                            placeholder: "2, 2, 1, 2, 2, 2, 1",
                            value: "{form_intervals}",
                            oninput: move |e| form_intervals.set(e.value())
                        }
                        p { class: "form-hint",
                            "Intervals must sum to the temperament's number of divisions (one period)."
                        }
                    }
                }
            }
        }
    }
}
