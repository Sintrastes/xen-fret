use dioxus::prelude::*;
use crate::components::Layout;
use crate::pages::{Home, Temperaments, Tunings, Scales, Chords, Preferences};

#[derive(Routable, Clone, Debug, PartialEq)]
#[rustfmt::skip]
pub enum Route {
    #[layout(Layout)]
        #[route("/")]
        Home {},
        #[route("/temperaments")]
        Temperaments {},
        #[route("/tunings")]
        Tunings {},
        #[route("/scales")]
        Scales {},
        #[route("/chords")]
        Chords {},
        #[route("/preferences")]
        Preferences {},
    #[end_layout]
    #[route("/:..segments")]
    NotFound { segments: Vec<String> },
}

#[component]
fn NotFound(segments: Vec<String>) -> Element {
    rsx! {
        div { class: "page",
            div { class: "empty-state",
                span { class: "empty-icon", "?" }
                h2 { "Page not found" }
                p { { format!("The page '{}' doesn't exist.", segments.join("/")) } }
                Link { to: Route::Home {}, class: "btn btn-primary", "Go Home" }
            }
        }
    }
}
