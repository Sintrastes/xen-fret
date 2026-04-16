use dioxus::desktop::{Config, WindowBuilder};

const BASE_CSS: &str = include_str!("../../assets/base.css");
const WEB_CSS: &str = include_str!("../../assets/web.css");

pub fn configure_window(wb: WindowBuilder) -> WindowBuilder {
    wb
}

pub fn configure_desktop(cfg: Config) -> Config {
    let css_head = format!("<style>{}</style><style>{}</style>", BASE_CSS, WEB_CSS);
    cfg.with_custom_head(css_head)
}

pub const TITLEBAR_HEIGHT_OFFSET: u32 = 28;
pub fn titlebar_height_offset() -> u32 { 28 }
