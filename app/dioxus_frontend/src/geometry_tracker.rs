use dioxus::desktop::{Config, WindowBuilder};
use dioxus::desktop::tao::dpi::{LogicalPosition, LogicalSize};
use dioxus::desktop::tao::window::Icon;
use app_common::storage;

pub use app_common::geometry::WindowGeometry;

/// Rasterize the bundled SVG icon at 256×256 and return a tao `Icon`.
/// Not used on macOS — the Dock icon comes from the app bundle (.icns), and
/// tao's with_window_icon() has no well-defined effect on that platform.
#[cfg(not(target_os = "macos"))]
fn load_window_icon() -> Option<Icon> {
    let svg_bytes = include_bytes!("../assets/xen-fret-icon.svg");
    let options = usvg::Options::default();
    let tree = usvg::Tree::from_data(svg_bytes, &options).ok()?;

    let size = 256u32;
    let mut pixmap = tiny_skia::Pixmap::new(size, size)?;
    let sx = size as f32 / tree.size().width();
    let sy = size as f32 / tree.size().height();
    resvg::render(&tree, tiny_skia::Transform::from_scale(sx, sy), &mut pixmap.as_mut());

    Icon::from_rgba(pixmap.take(), size, size).ok()
}

/// Build a `Config` with the saved window geometry applied to the `WindowBuilder`.
/// Position is set in the builder to prevent Dioxus's debug-mode state restoration
/// from interfering. Size uses the saved inner size.
pub fn build_config(app_fn: fn() -> dioxus::prelude::Element) -> ! {
    let geo = storage::load_window_geometry();
    let mut wb = WindowBuilder::new().with_title("Xen Fret");

    #[cfg(not(target_os = "macos"))]
    { wb = wb.with_window_icon(load_window_icon()); }

    // In debug mode, keep the window on top so it isn't hidden behind
    // the terminal during `dx serve` (matches `Config::new()` default).
    if cfg!(debug_assertions) {
        wb = wb.with_always_on_top(true);
    }

    if let Some(g) = geo {
        wb = wb
            .with_position(LogicalPosition::new(g.x as f64, g.y as f64))
            .with_inner_size(LogicalSize::new(g.width as f64, g.height as f64));
    }

    // Apply platform-specific window builder settings (e.g. transparent titlebar on macOS).
    wb = crate::platform::configure_window(wb);

    let cfg = Config::new().with_window(wb);
    // Apply platform-specific Config settings (e.g. custom CSS head on macOS).
    let cfg = crate::platform::configure_desktop(cfg);

    dioxus::LaunchBuilder::new()
        .with_cfg(cfg)
        .launch(app_fn);

    // LaunchBuilder::launch() blocks; this is unreachable but satisfies the
    // return type so callers can `return window_geometry::launch(App)`.
    unreachable!()
}

/// Hook: call inside a desktop `#[component]` to persist window geometry on
/// move, resize, close, and destroy events.
pub fn use_window_geometry_tracker() {
    use std::rc::Rc;
    use std::cell::Cell;
    use std::time::{Duration, Instant};
    use dioxus::desktop::use_wry_event_handler;
    use dioxus::desktop::tao::event::{Event, WindowEvent};
    use crate::geometry_tracker::WindowGeometry;

    let last_save = Rc::new(Cell::new(Instant::now() - Duration::from_secs(10)));

    use_wry_event_handler(move |event, _| {
        let Event::WindowEvent { event, .. } = event else { return };

        let should_save = match event {
            WindowEvent::CloseRequested | WindowEvent::Destroyed => true,
            WindowEvent::Moved(_) | WindowEvent::Resized(_) => {
                let now = Instant::now();
                if now.duration_since(last_save.get()) >= Duration::from_millis(500) {
                    last_save.set(now);
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
            let Ok(phys_pos) = win.outer_position() else { return };
            let scale = win.scale_factor();
            // tao's inner_size() is broken on macOS — use outer_size() minus
            // the title bar height (28 px), matching Dioxus's own workaround.
            let phys_size = win.outer_size();
            let title_bar: u32 = if win.is_decorated() {
                crate::platform::titlebar_height_offset()
            } else {
                0
            };
            let logical_pos = phys_pos.to_logical::<f64>(scale);
            let logical_size = phys_size.to_logical::<u32>(scale);
            let geo = WindowGeometry {
                x: logical_pos.x as i32,
                y: logical_pos.y as i32,
                width: logical_size.width,
                height: logical_size.height.saturating_sub(title_bar),
            };
            storage::save_window_geometry(&geo);
        }
    });
}
