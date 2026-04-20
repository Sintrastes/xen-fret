use crate::components::Modal;
use crate::state::APP_STATE;
use dioxus::prelude::*;
use num::rational::Ratio;
use xen_theory::temperament::Temperament;

#[component]
pub fn Temperaments() -> Element {
    let mut show_add = use_signal(|| false);
    let mut edit_idx: Signal<Option<usize>> = use_signal(|| None);

    // Form state
    let mut form_name = use_signal(|| String::new());
    let mut form_divisions = use_signal(|| String::from("12"));
    let mut form_period = use_signal(|| String::from("2/1"));

    let temperaments: Vec<(usize, String, u32, String)> = APP_STATE
        .read()
        .temperaments
        .iter()
        .enumerate()
        .map(|(i, t)| (i, t.name.clone(), t.divisions, t.period_str()))
        .collect();

    let open_add = move |_| {
        form_name.set(String::new());
        form_divisions.set("12".into());
        form_period.set("2/1".into());
        edit_idx.set(None);
        show_add.set(true);
    };

    let mut open_edit = move |idx: usize| {
        let s = APP_STATE.read();
        if let Some(t) = s.temperaments.get(idx) {
            form_name.set(t.name.clone());
            form_divisions.set(t.divisions.to_string());
            form_period.set(t.period_str());
            drop(s);
            edit_idx.set(Some(idx));
            show_add.set(true);
        }
    };

    let save = move |_| {
        let name = form_name.read().trim().to_string();
        let divisions: u32 = form_divisions.read().trim().parse().unwrap_or(12);
        let period_str = form_period.read().trim().to_string();
        let period = parse_ratio(&period_str)
            .map(|(n, d)| Ratio::new(n, d))
            .unwrap_or(Ratio::new(2, 1));

        {
            let mut s = APP_STATE.write();
            if let Some(idx) = *edit_idx.read() {
                s.update_temperament(idx, name, divisions, period);
            } else {
                s.add_temperament(Temperament {
                    name,
                    divisions,
                    period,
                });
            }
        }
        app_common::storage::save(&*APP_STATE.read());
    };

    let delete = move |idx: usize| {
        APP_STATE.write().delete_temperament(idx);
        app_common::storage::save(&*APP_STATE.read());
    };

    rsx! {
        div { class: "page",
            // Page header
            div { class: "page-header",
                div {
                    h1 { class: "page-title", "Temperaments" }
                    p { class: "page-subtitle",
                        "Define equal-division tuning systems with their periods."
                    }
                }
                button {
                    class: "btn btn-primary",
                    onclick: open_add,
                    "+ New Temperament"
                }
            }

            // Table / list
            if temperaments.is_empty() {
                div { class: "empty-state",
                    span { class: "empty-icon", "𝄇" }
                    p { "No temperaments defined yet." }
                    button { class: "btn btn-primary", onclick: open_add, "Add your first temperament" }
                }
            } else {
                div { class: "card",
                    table { class: "data-table",
                        thead {
                            tr {
                                th { "Name" }
                                th { "Divisions" }
                                th { "Period" }
                                th { class: "col-actions", "Actions" }
                            }
                        }
                        tbody {
                            for (idx, name, divisions, period) in &temperaments {
                                tr {
                                    td {
                                        span { class: "item-name", "{name}" }
                                    }
                                    td {
                                        span { class: "badge", "{divisions}-EDO" }
                                    }
                                    td { "{period}" }
                                    td { class: "col-actions",
                                        button {
                                            class: "btn-icon",
                                            title: "Edit",
                                            onclick: {
                                                let idx = *idx;
                                                move |_| open_edit(idx)
                                            },
                                            "✎"
                                        }
                                        button {
                                            class: "btn-icon btn-icon-danger",
                                            title: "Delete",
                                            onclick: {
                                                let idx = *idx;
                                                move |_| delete(idx)
                                            },
                                            "✕"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Add / Edit modal
            Modal {
                show: show_add,
                title: if edit_idx.read().is_some() { "Edit Temperament".to_string() } else { "New Temperament".to_string() },
                on_confirm: save,
                confirm_label: Some("Save".into()),

                div { class: "form-stack",
                    div { class: "form-group",
                        label { class: "form-label", "Name" }
                        input {
                            class: "form-input",
                            r#type: "text",
                            placeholder: "e.g. 12-EDO",
                            value: "{form_name}",
                            oninput: move |e| form_name.set(e.value())
                        }
                    }
                    div { class: "form-row",
                        div { class: "form-group",
                            label { class: "form-label", "Divisions" }
                            input {
                                class: "form-input",
                                r#type: "number",
                                min: "2",
                                max: "1200",
                                placeholder: "12",
                                value: "{form_divisions}",
                                oninput: move |e| form_divisions.set(e.value())
                            }
                        }
                        div { class: "form-group",
                            label { class: "form-label", "Period (ratio)" }
                            input {
                                class: "form-input",
                                r#type: "text",
                                placeholder: "2/1",
                                value: "{form_period}",
                                oninput: move |e| form_period.set(e.value())
                            }
                        }
                    }
                    p { class: "form-hint",
                        "Period is usually 2/1 (octave). Use 3/2 for Bohlen-Pierce, etc."
                    }
                }
            }
        }
    }
}

fn parse_ratio(s: &str) -> Option<(u32, u32)> {
    let parts: Vec<&str> = s.trim().split('/').collect();
    match parts.as_slice() {
        [n] => n.trim().parse().ok().map(|n| (n, 1)),
        [n, d] => {
            let n = n.trim().parse().ok()?;
            let d = d.trim().parse().ok()?;
            Some((n, d))
        }
        _ => None,
    }
}
