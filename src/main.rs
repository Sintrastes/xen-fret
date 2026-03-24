#![allow(non_snake_case)]

pub mod notation;
pub mod scala;
mod models;
mod state;
mod storage;
mod pages;
mod components;
mod routes;
pub mod fretboard;
pub mod theory;
pub mod audio;
pub mod pitch_tracking;
mod theme;
#[cfg(not(target_arch = "wasm32"))]
mod geometry_tracker;
#[cfg(not(target_arch = "wasm32"))]
mod platform;

use dioxus::prelude::*;
use routes::Route;
use state::{AppState, APP_STATE};

fn main() {
    #[cfg(not(target_arch = "wasm32"))]
    return geometry_tracker::build_config(App);
    #[cfg(target_arch = "wasm32")]
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    use_context_provider(|| Signal::new(AppState::default()));

    // Auto-save to storage whenever APP_STATE changes.
    use_effect(move || {
        storage::save(&*APP_STATE.read());
    });

    #[cfg(not(target_arch = "wasm32"))]
    geometry_tracker::use_window_geometry_tracker();

    rsx! {
        document::Stylesheet { href: asset!("/assets/style.css") }
        Router::<Route> {}
    }
}
