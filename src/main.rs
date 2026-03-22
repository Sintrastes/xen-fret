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

use dioxus::prelude::*;
use routes::Route;
use state::{AppState, APP_STATE};

fn main() {
    #[cfg(not(target_arch = "wasm32"))]
    {
        use dioxus::desktop::{Config, WindowBuilder};

        let geo = storage::load_window_geometry();
        let mut wb = WindowBuilder::new().with_title("Xen Fret");
        // In debug mode, keep the window on top (matches Config::new() default
        // behavior so the window isn't hidden behind the terminal during dx serve).
        if cfg!(debug_assertions) {
            wb = wb.with_always_on_top(true);
        }
        if let Some(g) = geo {
            use dioxus::desktop::tao::dpi::{LogicalPosition, LogicalSize};
            // Set both position and size. Position is set here to prevent
            // Dioxus's debug-mode state restoration from interfering.
            wb = wb
                .with_position(LogicalPosition::new(g.x as f64, g.y as f64))
                .with_inner_size(LogicalSize::new(g.width as f64, g.height as f64));
        }
        dioxus::LaunchBuilder::new()
            .with_cfg(Config::new().with_window(wb))
            .launch(App);
        return;
    }
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

    // Persist window geometry on desktop via window events.
    #[cfg(not(target_arch = "wasm32"))]
    {
        // Throttle Moved/Resized saves; always save on close/destroy.
        let last_move_save = std::rc::Rc::new(std::cell::Cell::new(
            std::time::Instant::now() - std::time::Duration::from_secs(10),
        ));

        dioxus::desktop::use_wry_event_handler(move |event, _| {
            use dioxus::desktop::tao::event::{Event, WindowEvent};
            let Event::WindowEvent { event, .. } = event else { return };

            let should_save = match event {
                // Always save before the window closes / is destroyed.
                WindowEvent::CloseRequested | WindowEvent::Destroyed => {
                    eprintln!("[xen-fret] {:?} — saving geometry", event);
                    true
                }
                // Throttle move/resize to avoid a DB write for every pixel dragged.
                WindowEvent::Moved(_) | WindowEvent::Resized(_) => {
                    let now = std::time::Instant::now();
                    if now.duration_since(last_move_save.get())
                        >= std::time::Duration::from_millis(500)
                    {
                        last_move_save.set(now);
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            };

            if should_save {
                let ctx = dioxus::desktop::window();
                let win = &ctx.window;
                let pos_result = win.outer_position();
                let scale = win.scale_factor();
                // tao's inner_size() is broken on macOS — use outer_size()
                // minus the title bar height (28px), matching Dioxus's own workaround.
                let phys_size = win.outer_size();
                let title_bar: u32 = if win.is_decorated() { 28 } else { 0 };
                let Ok(phys_pos) = pos_result else { return };
                let logical_pos = phys_pos.to_logical::<f64>(scale);
                let logical_size = phys_size.to_logical::<u32>(scale);
                let geo = models::WindowGeometry {
                    x: logical_pos.x as i32,
                    y: logical_pos.y as i32,
                    width: logical_size.width,
                    height: logical_size.height.saturating_sub(title_bar),
                };
                eprintln!(
                    "[xen-fret] saving geometry: {:?}",
                    geo
                );
                storage::save_window_geometry(&geo);
            }
        });
    }

    rsx! {
        document::Stylesheet { href: asset!("/assets/style.css") }
        Router::<Route> {}
    }
}
