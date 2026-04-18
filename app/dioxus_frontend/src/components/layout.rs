use app_common::preferences::ThemeMode;
use crate::routes::Route;
use crate::state::APP_STATE;
use crate::theme::use_system_dark_watcher;
use dioxus::prelude::*;

/// Set the Android status bar (system tray) color to match the sidebar header.
/// sidebar-bg token values from assets/web.css:
///   light → #0f2544   dark → #071526
#[cfg(target_os = "android")]
fn set_status_bar_color(dark: bool) {
    use jni::objects::{JObject, JValue};
    use jni::JavaVM;

    let rgb: u32 = if dark { 0x071526 } else { 0x0f2544 };
    let argb: i32 = (0xFF000000u32 | rgb) as i32;

    let ctx = unsafe { ndk_context::android_context() };
    let vm = match unsafe { JavaVM::from_raw(ctx.vm().cast()) } {
        Ok(v) => v,
        Err(_) => return,
    };
    let mut env = match vm.attach_current_thread() {
        Ok(e) => e,
        Err(_) => return,
    };
    let activity = unsafe { JObject::from_raw(ctx.context().cast()) };
    let window = match env
        .call_method(&activity, "getWindow", "()Landroid/view/Window;", &[])
        .and_then(|v| v.l())
    {
        Ok(w) => w,
        Err(_) => return,
    };
    let _ = env.call_method(
        &window,
        "setStatusBarColor",
        "(I)V",
        &[JValue::Int(argb)],
    );
}

/// Start a native window drag on mousedown (desktop only).
/// This is the Dioxus-recommended way — CSS `-webkit-app-region: drag`
/// does not work reliably in the webview.
#[cfg(not(target_arch = "wasm32"))]
fn start_drag(_evt: MouseEvent) {
    dioxus::desktop::window().drag();
}

#[cfg(target_arch = "wasm32")]
fn start_drag(_evt: MouseEvent) {}

const SIDEBAR_TOGGLE_ICON: &str = r#"<svg width="18" height="18" viewBox="0 0 16 16" fill="none">
  <rect x="1" y="2" width="14" height="12" rx="2" stroke="currentColor" stroke-width="1.5"/>
  <line x1="5.5" y1="2" x2="5.5" y2="14" stroke="currentColor" stroke-width="1.5"/>
  <line x1="2.5" y1="5.5" x2="4.5" y2="5.5" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/>
  <line x1="2.5" y1="8" x2="4.5" y2="8" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/>
  <line x1="2.5" y1="10.5" x2="4.5" y2="10.5" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/>
</svg>"#;

const HAMBURGER_ICON: &str = r#"<svg width="20" height="20" viewBox="0 0 20 20" fill="none">
  <line x1="3" y1="5" x2="17" y2="5" stroke="currentColor" stroke-width="1.75" stroke-linecap="round"/>
  <line x1="3" y1="10" x2="17" y2="10" stroke="currentColor" stroke-width="1.75" stroke-linecap="round"/>
  <line x1="3" y1="15" x2="17" y2="15" stroke="currentColor" stroke-width="1.75" stroke-linecap="round"/>
</svg>"#;

#[component]
pub fn Layout() -> Element {
    use_system_dark_watcher();

    let dark_mode = APP_STATE.read().effective_dark_mode();

    // Keep Android status bar color in sync with the app theme.
    #[cfg(target_os = "android")]
    {
        let dark = dark_mode;
        use_effect(move || set_status_bar_color(dark));
    }
    let mut sidebar_collapsed = use_signal(|| false);
    let mut mobile_menu_open = use_signal(|| false);

    rsx! {
        div {
            class: "app-root",
            "data-theme": if dark_mode { "dark" } else { "light" },

            // Mobile menu backdrop
            if mobile_menu_open() {
                div {
                    class: "mobile-overlay",
                    onclick: move |_| mobile_menu_open.set(false),
                }
            }

            // Sidebar (desktop) / mobile drawer
            aside {
                class: if sidebar_collapsed() {
                    "sidebar collapsed"
                } else if mobile_menu_open() {
                    "sidebar mobile-open"
                } else {
                    "sidebar"
                },

                if cfg!(target_arch = "wasm32") || cfg!(target_os = "android") {
                    div { class: "sidebar-logo",
                        span { class: "logo-icon", "♩" }
                        span { class: "logo-text", "Xen Fret" }
                        button {
                            class: "sidebar-close",
                            onclick: move |_| mobile_menu_open.set(false),
                            "✕"
                        }
                    }
                }
                // Desktop only: drag region for the titlebar area above sidebar nav
                if !cfg!(target_arch = "wasm32") && !cfg!(target_os = "android") {
                    div {
                        class: "titlebar-drag-region sidebar-drag",
                        onmousedown: start_drag,
                    }
                }

                nav {
                    class: "sidebar-nav",
                    onclick: move |_| mobile_menu_open.set(false),
                    NavLink { to: Route::Home {}, icon: "⌂", label: "Diagram" }
                    NavLink { to: Route::Temperaments {}, icon: "♩", label: "Temperaments" }
                    NavLink { to: Route::Tunings {}, icon: "♪", label: "Tunings" }
                    NavLink { to: Route::Scales {}, icon: "♫", label: "Scales" }
                    NavLink { to: Route::Chords {}, icon: "♬", label: "Chords" }
                    NavLink { to: Route::BackingTrack {}, icon: "𝄞", label: "Track" }
                }

                div {
                    class: "sidebar-footer",
                    onclick: move |_| mobile_menu_open.set(false),
                    Link {
                        to: Route::Preferences {},
                        class: "sidebar-link preferences-link",
                        span { class: "nav-icon", "⚙" }
                        span { class: "nav-label", "Preferences" }
                    }
                }
            }

            // Sidebar toggle — desktop only (not web, not Android).
            if !cfg!(target_arch = "wasm32") && !cfg!(target_os = "android") {
                button {
                    class: "sidebar-toggle",
                    dangerous_inner_html: SIDEBAR_TOGGLE_ICON,
                    onclick: move |_| sidebar_collapsed.set(!sidebar_collapsed()),
                }
            }

            // Mobile top bar
            header { class: "mobile-header",
                if cfg!(target_arch = "wasm32") || cfg!(target_os = "android") {
                    button {
                        class: "mobile-hamburger",
                        dangerous_inner_html: HAMBURGER_ICON,
                        onclick: move |_| mobile_menu_open.set(true),
                    }
                }
                button {
                    class: "theme-toggle-mobile",
                    onclick: move |_| {
                        let next = match APP_STATE.read().preferences.theme_mode {
                            ThemeMode::System => ThemeMode::Light,
                            ThemeMode::Light => ThemeMode::Dark,
                            ThemeMode::Dark => ThemeMode::System,
                        };
                        APP_STATE.write().preferences.theme_mode = next;
                    },
                    match APP_STATE.read().preferences.theme_mode {
                        ThemeMode::System => "◑",
                        ThemeMode::Light => "☀",
                        ThemeMode::Dark => "☾",
                    }
                }
            }

            // Main content
            main { class: "main-content",
                // Desktop only: toolbar drag region
                if !cfg!(target_arch = "wasm32") && !cfg!(target_os = "android") {
                    div {
                        class: "titlebar-drag-region main-drag",
                        onmousedown: start_drag,
                    }
                }
                Outlet::<Route> {}
            }
        }
    }
}

// Use Link's built-in active_class instead of use_route to avoid potential panics
#[component]
fn NavLink(to: Route, icon: &'static str, label: &'static str) -> Element {
    rsx! {
        Link {
            to,
            class: "sidebar-link",
            active_class: "active",
            span { class: "nav-icon", "{icon}" }
            span { class: "nav-label", "{label}" }
        }
    }
}

#[component]
fn MobileNavLink(to: Route, icon: &'static str, label: &'static str) -> Element {
    rsx! {
        Link {
            to,
            class: "mobile-nav-link",
            active_class: "active",
            span { class: "mobile-nav-icon", "{icon}" }
            span { class: "mobile-nav-label", "{label}" }
        }
    }
}
