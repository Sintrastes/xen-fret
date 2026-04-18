use dioxus::prelude::*;

// ── Shared context ────────────────────────────────────────────────────────────

/// State shared between the compound dropdown components via Dioxus context.
#[derive(Clone, Copy)]
struct DropdownState {
    open: Signal<bool>,
    focused_idx: Signal<Option<usize>>,
    item_count: Signal<usize>,
}

// ── Primitive components ──────────────────────────────────────────────────────

/// Root wrapper. Provides the shared dropdown context to all descendants.
#[component]
pub fn DropdownMenu(children: Element) -> Element {
    let open = use_signal(|| false);
    let focused_idx = use_signal(|| Option::<usize>::None);
    let item_count = use_signal(|| 0usize);
    provide_context(DropdownState { open, focused_idx, item_count });
    rsx! {
        div { class: "dropdown", {children} }
    }
}

/// The clickable trigger that toggles the dropdown open/closed.
#[component]
pub fn DropdownMenuTrigger(children: Element) -> Element {
    let mut state = use_context::<DropdownState>();

    let on_key = move |e: KeyboardEvent| {
        match e.key() {
            Key::Escape => {
                state.open.set(false);
                state.focused_idx.set(None);
                e.prevent_default();
            }
            Key::ArrowDown => {
                if !*state.open.read() {
                    state.open.set(true);
                    state.focused_idx.set(Some(0));
                } else {
                    let n = *state.item_count.read();
                    if n > 0 {
                        let next = (*state.focused_idx.read())
                            .map(|i| if i + 1 >= n { 0 } else { i + 1 })
                            .unwrap_or(0);
                        state.focused_idx.set(Some(next));
                    }
                }
                e.prevent_default();
            }
            Key::ArrowUp => {
                if !*state.open.read() {
                    state.open.set(true);
                }
                let n = *state.item_count.read();
                if n > 0 {
                    let prev = (*state.focused_idx.read())
                        .map(|i| if i == 0 { n - 1 } else { i - 1 })
                        .unwrap_or(n - 1);
                    state.focused_idx.set(Some(prev));
                }
                e.prevent_default();
            }
            Key::Home => {
                if *state.open.read() { state.focused_idx.set(Some(0)); }
                e.prevent_default();
            }
            Key::End => {
                if *state.open.read() {
                    let n = *state.item_count.read();
                    if n > 0 { state.focused_idx.set(Some(n - 1)); }
                }
                e.prevent_default();
            }
            Key::Enter => {
                if !*state.open.read() {
                    state.open.set(true);
                    state.focused_idx.set(Some(0));
                    e.prevent_default();
                }
            }
            Key::Character(ref c) if c == " " => {
                if !*state.open.read() {
                    state.open.set(true);
                    state.focused_idx.set(Some(0));
                    e.prevent_default();
                }
            }
            _ => {}
        }
    };

    rsx! {
        button {
            class: if *state.open.read() { "dropdown-trigger dropdown-trigger--open" } else { "dropdown-trigger" },
            r#type: "button",
            role: "combobox",
            aria_expanded: "{state.open}",
            aria_haspopup: "listbox",
            tabindex: "0",
            onclick: move |_| {
                let is_open = *state.open.read();
                state.open.set(!is_open);
                if !is_open {
                    state.focused_idx.set(Some(0));
                } else {
                    state.focused_idx.set(None);
                }
            },
            onkeydown: on_key,
            {children}
            span {
                class: if *state.open.read() { "dropdown-chevron dropdown-chevron--open" } else { "dropdown-chevron" },
                dangerous_inner_html: r#"<svg width="12" height="12" viewBox="0 0 12 12" aria-hidden="true"><path fill="currentColor" d="M6 8L1 3h10z"/></svg>"#,
            }
        }
    }
}

/// The panel that appears below the trigger. Only renders when open.
/// Wrap your `DropdownMenuItem` elements in this.
#[component]
pub fn DropdownMenuContent(children: Element) -> Element {
    let mut state = use_context::<DropdownState>();

    if !*state.open.read() {
        return rsx! {};
    }

    rsx! {
        // Invisible full-viewport backdrop catches clicks outside the dropdown.
        div {
            class: "dropdown-backdrop",
            onclick: move |_| {
                state.open.set(false);
                state.focused_idx.set(None);
            },
        }
        div {
            class: "dropdown-content",
            role: "listbox",
            onclick: |e| e.stop_propagation(),
            {children}
        }
    }
}

/// A single selectable item inside `DropdownMenuContent`.
#[derive(Props, Clone, PartialEq)]
pub struct DropdownMenuItemProps {
    /// Zero-based position; used for keyboard focus tracking.
    pub index: usize,
    /// Value passed to `on_select` when this item is chosen.
    pub value: String,
    /// Called when the item is selected (clicked or Enter while focused).
    pub on_select: EventHandler<String>,
    /// Whether this item reflects the currently active selection.
    #[props(default = false)]
    pub selected: bool,
    pub children: Element,
}

#[component]
pub fn DropdownMenuItem(props: DropdownMenuItemProps) -> Element {
    let DropdownMenuItemProps { index, value, on_select, selected, children } = props;
    let mut state = use_context::<DropdownState>();

    // Register this item's existence so the trigger knows the total count.
    let new_count = index + 1;
    if *state.item_count.read() < new_count {
        state.item_count.set(new_count);
    }

    let is_focused = *state.focused_idx.read() == Some(index);

    let cls = match (selected, is_focused) {
        (true, true)  => "dropdown-item dropdown-item--selected dropdown-item--focused",
        (true, false) => "dropdown-item dropdown-item--selected",
        (false, true) => "dropdown-item dropdown-item--focused",
        (false, false) => "dropdown-item",
    };

    rsx! {
        div {
            class: "{cls}",
            role: "option",
            aria_selected: "{selected}",
            onmouseenter: move |_| state.focused_idx.set(Some(index)),
            onclick: move |_| {
                on_select.call(value.clone());
                state.open.set(false);
                state.focused_idx.set(None);
            },
            {children}
            if selected {
                span { class: "dropdown-item-check", "\u{2713}" }
            }
        }
    }
}

// ── Convenience wrapper ───────────────────────────────────────────────────────

/// A pre-built select control built from the dropdown primitives.
/// Accepts a flat list of `(value, label)` pairs.
#[derive(Props, Clone, PartialEq)]
pub struct SelectProps {
    pub value: String,
    pub options: Vec<(String, String)>,
    pub on_change: EventHandler<String>,
    #[props(optional)]
    pub placeholder: Option<String>,
}

#[component]
pub fn Select(props: SelectProps) -> Element {
    let SelectProps { value, options, on_change, placeholder } = props;

    let display = options.iter()
        .find(|(v, _)| *v == value)
        .map(|(_, l)| l.clone())
        .unwrap_or_else(|| placeholder.unwrap_or_default());

    rsx! {
        DropdownMenu {
            DropdownMenuTrigger {
                span { class: "dropdown-trigger-label", "{display}" }
            }
            DropdownMenuContent {
                for (idx, (val, label)) in options.iter().enumerate() {
                    DropdownMenuItem {
                        index: idx,
                        value: val.clone(),
                        selected: *val == value,
                        on_select: {
                            let val = val.clone();
                            move |_| on_change.call(val.clone())
                        },
                        "{label}"
                    }
                }
            }
        }
    }
}
