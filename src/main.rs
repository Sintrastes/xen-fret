#![allow(non_snake_case)]

pub mod notation;
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

use dioxus::prelude::*;
use routes::Route;
use state::{AppState, APP_STATE};

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    use_context_provider(|| Signal::new(AppState::default()));

    // Auto-save to storage whenever APP_STATE changes.
    use_effect(move || {
        storage::save(&*APP_STATE.read());
    });

    rsx! {
        document::Stylesheet { href: asset!("/assets/style.css") }
        Router::<Route> {}
    }
}
