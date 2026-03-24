use dioxus::desktop::{Config, WindowBuilder};

pub fn configure_window(wb: WindowBuilder) -> WindowBuilder {
    wb
}

pub fn configure_desktop(cfg: Config) -> Config {
    cfg
}

/// Title bar height to subtract when computing inner size from outer size.
/// On most platforms the decorated title bar is ~28 logical pixels.
pub const TITLEBAR_HEIGHT_OFFSET: u32 = 28;
