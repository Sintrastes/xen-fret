use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct ModalProps {
    pub show: Signal<bool>,
    pub title: String,
    #[props(optional)]
    pub on_confirm: Option<EventHandler<()>>,
    #[props(optional)]
    pub confirm_label: Option<String>,
    #[props(optional)]
    pub on_cancel: Option<EventHandler<()>>,
    pub children: Element,
}

#[component]
pub fn Modal(props: ModalProps) -> Element {
    let ModalProps { mut show, title, on_confirm, confirm_label, on_cancel, children } = props;

    if !show() {
        return rsx! {};
    }

    rsx! {
        div {
            class: "modal-backdrop",
            onclick: move |_| {
                if let Some(h) = &on_cancel { h.call(()); }
                show.set(false);
            },

            div {
                class: "modal",
                onclick: |e| e.stop_propagation(),

                // Header
                div { class: "modal-header",
                    h2 { class: "modal-title", "{title}" }
                    button {
                        class: "modal-close",
                        onclick: move |_| {
                            if let Some(h) = &on_cancel { h.call(()); }
                            show.set(false);
                        },
                        "✕"
                    }
                }

                // Body
                div { class: "modal-body",
                    {children}
                }

                // Footer (if actions provided)
                if on_confirm.is_some() {
                    div { class: "modal-footer",
                        button {
                            class: "btn btn-ghost",
                            onclick: move |_| {
                                if let Some(h) = &on_cancel { h.call(()); }
                                show.set(false);
                            },
                            "Cancel"
                        }
                        button {
                            class: "btn btn-primary",
                            onclick: move |_| {
                                if let Some(handler) = &on_confirm {
                                    handler.call(());
                                }
                                show.set(false);
                            },
                            { confirm_label.as_deref().unwrap_or("Save") }
                        }
                    }
                }
            }
        }
    }
}
