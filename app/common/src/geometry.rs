use serde::{Deserialize, Serialize};

/// Persisted desktop window position and size (logical pixels, DPI-independent).
/// Tracked separately from `Preferences` — saved on every move/resize.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub struct WindowGeometry {
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32,
}
