use crate::models::ThemeMode;
use crate::routes::Route;
use crate::state::APP_STATE;
use crate::theme::use_system_dark_watcher;
use dioxus::prelude::*;

#[component]
pub fn Layout() -> Element {
    use_system_dark_watcher();

    let dark_mode = APP_STATE.read().effective_dark_mode();

    rsx! {
        div {
            class: "app-root",
            "data-theme": if dark_mode { "dark" } else { "light" },

            // Sidebar (desktop)
            aside { class: "sidebar",
                div { class: "sidebar-logo",
                    span { class: "logo-icon", "♩" }
                    span { class: "logo-text", "Xen Fret" }
                }

                nav { class: "sidebar-nav",
                    NavLink { to: Route::Home {}, icon: "⌂", label: "Diagram" }
                    NavLink { to: Route::Temperaments {}, icon: "♩", label: "Temperaments" }
                    NavLink { to: Route::Tunings {}, icon: "♪", label: "Tunings" }
                    NavLink { to: Route::Scales {}, icon: "♫", label: "Scales" }
                    NavLink { to: Route::Chords {}, icon: "♬", label: "Chords" }
                }

                div { class: "sidebar-footer",
                    Link {
                        to: Route::Preferences {},
                        class: "sidebar-link preferences-link",
                        span { class: "nav-icon", "⚙" }
                        span { class: "nav-label", "Preferences" }
                    }
                }
            }

            // Mobile top bar
            header { class: "mobile-header",
                span { class: "logo-icon", "♩" }
                span { class: "logo-text", "Xen Fret" }
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
                Outlet::<Route> {}
            }

            // Mobile bottom nav
            nav { class: "mobile-nav",
                MobileNavLink { to: Route::Home {}, icon: "⌂", label: "Diagram" }
                MobileNavLink { to: Route::Temperaments {}, icon: "♩", label: "Temps" }
                MobileNavLink { to: Route::Tunings {}, icon: "♪", label: "Tunings" }
                MobileNavLink { to: Route::Scales {}, icon: "♫", label: "Scales" }
                MobileNavLink { to: Route::Chords {}, icon: "♬", label: "Chords" }
                MobileNavLink { to: Route::Preferences {}, icon: "⚙", label: "Settings" }
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
