use dioxus::prelude::*;

/// A text input that filters a dropdown list as the user types.
/// Same `(value, label)` API as `Select`.
#[component]
pub fn Combobox(
    value: String,
    options: Vec<(String, String)>,
    on_change: EventHandler<String>,
    #[props(optional)] placeholder: Option<String>,
) -> Element {
    let current_label = options.iter()
        .find(|(k, _)| k == &value)
        .map(|(_, v)| v.clone())
        .unwrap_or_default();

    // `filter` holds what the user is typing. Only used while open.
    let mut filter = use_signal(String::new);
    let mut open = use_signal(|| false);
    let mut hi: Signal<Option<usize>> = use_signal(|| None); // highlighted index
    let mut composing = use_signal(|| false); // true while IME composition is in progress

    // When closed, display the selected label. When open, display the filter text.
    let display = if *open.read() { filter.read().clone() } else { current_label.clone() };

    // Build filtered list only while open.
    let filter_lc = filter.read().to_lowercase();
    let filtered: Vec<(String, String)> = if *open.read() {
        options.iter()
            .filter(|(_, label)| label.to_lowercase().contains(&filter_lc))
            .cloned()
            .collect()
    } else {
        vec![]
    };
    let n = filtered.len();

    let mut do_open = move || {
        filter.set(String::new());
        open.set(true);
        hi.set(None);
    };
    let mut do_close = move || {
        open.set(false);
        hi.set(None);
    };

    rsx! {
        div { class: "cmb-wrap",
            // Backdrop — captures outside clicks to close
            if *open.read() {
                div {
                    class: "cmb-backdrop",
                    onclick: move |_| do_close(),
                }
            }

            // Input row
            div { class: if *open.read() { "cmb-field cmb-field--open" } else { "cmb-field" },
                input {
                    class: "cmb-input",
                    r#type: "text",
                    value: "{display}",
                    placeholder: "{placeholder.as_deref().unwrap_or(\"\")}",

                    onfocus: move |_| do_open(),

                    oninput: move |e| {
                        // Suppress filtering while IME composition is in progress.
                        if *composing.read() { return; }
                        filter.set(e.value());
                        open.set(true);
                        hi.set(Some(0));
                    },

                    oncompositionstart: move |_| composing.set(true),

                    oncompositionend: move |e: CompositionEvent| {
                        composing.set(false);
                        // Apply the completed composition result as the filter.
                        let text = e.data().data();
                        filter.set(text);
                        open.set(true);
                        hi.set(Some(0));
                    },

                    onkeydown: {
                        let opts = options.clone();
                        move |e: KeyboardEvent| {
                            // Don't act on keys fired during IME composition.
                            if e.is_composing() { return; }
                            match e.key() {
                                Key::Escape => {
                                    do_close();
                                    e.prevent_default();
                                }
                                Key::Enter => {
                                    let f = filter.read().to_lowercase();
                                    let matched: Vec<_> = opts.iter()
                                        .filter(|(_, l)| l.to_lowercase().contains(&f))
                                        .collect();
                                    let idx = hi.read().unwrap_or(0);
                                    if let Some((k, _)) = matched.get(idx) {
                                        on_change.call(k.to_string());
                                        do_close();
                                    }
                                    e.prevent_default();
                                }
                                Key::ArrowDown => {
                                    if !*open.read() {
                                        do_open();
                                    } else {
                                        let next = (*hi.read())
                                            .map(|h| (h + 1).min(n.saturating_sub(1)))
                                            .unwrap_or(0);
                                        hi.set(Some(next));
                                    }
                                    e.prevent_default();
                                }
                                Key::ArrowUp => {
                                    let prev = (*hi.read())
                                        .map(|h| h.saturating_sub(1))
                                        .unwrap_or(0);
                                    hi.set(Some(prev));
                                    e.prevent_default();
                                }
                                _ => {}
                            }
                        }
                    },
                }

                // Chevron toggle button
                button {
                    class: "cmb-toggle",
                    r#type: "button",
                    tabindex: "-1",
                    onclick: move |_| {
                        if *open.read() { do_close(); } else { do_open(); }
                    },
                    dangerous_inner_html: r#"<svg width="10" height="10" viewBox="0 0 10 10" aria-hidden="true"><path fill="currentColor" d="M5 7L0 2h10z"/></svg>"#,
                }
            }

            // Dropdown list
            if *open.read() {
                div { class: "cmb-list",
                    if filtered.is_empty() {
                        div { class: "cmb-no-match", "No matches" }
                    }
                    for (idx, (k, label)) in filtered.iter().enumerate() {
                        {
                            let k = k.clone();
                            let label = label.clone();
                            let is_hi  = *hi.read() == Some(idx);
                            let is_sel = k == value;
                            let mut cls = "cmb-item".to_string();
                            if is_sel { cls.push_str(" cmb-item--selected"); }
                            if is_hi  { cls.push_str(" cmb-item--hi"); }
                            rsx! {
                                div {
                                    class: "{cls}",
                                    onmouseenter: move |_| hi.set(Some(idx)),
                                    // prevent blur before click fires
                                    onmousedown: move |e| e.prevent_default(),
                                    onclick: move |_| {
                                        on_change.call(k.clone());
                                        do_close();
                                    },
                                    if is_sel {
                                        span { class: "cmb-check", "✓" }
                                    }
                                    "{label}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
