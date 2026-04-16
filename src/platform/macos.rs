use dioxus::desktop::tao::dpi::LogicalPosition;
use dioxus::desktop::tao::platform::macos::WindowBuilderExtMacOS;
use dioxus::desktop::{Config, WindowBuilder};

const BASE_CSS: &str = include_str!("../../assets/base.css");
const MACOS_CSS: &str = include_str!("../../assets/macos.css");

pub fn configure_window(wb: WindowBuilder) -> WindowBuilder {
    wb.with_titlebar_transparent(true)
        .with_title_hidden(true)
        .with_fullsize_content_view(true)
        .with_traffic_light_inset(LogicalPosition::new(16.0, 26.0))
}

pub fn configure_desktop(cfg: Config) -> Config {
    let css_head = format!("<style>{}</style><style>{}</style>", BASE_CSS, MACOS_CSS);
    cfg.with_background_color((0, 0, 0, 0)).with_custom_head(css_head)
}

/// With fullsize content view the content extends behind the titlebar,
/// so no height offset is needed when computing inner size.
pub const TITLEBAR_HEIGHT_OFFSET: u32 = 0;
pub fn titlebar_height_offset() -> u32 { 0 }
