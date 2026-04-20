use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub use fretboard_diagrams::{Color, DiagramColors, FretStyle};

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Default)]
pub enum ThemeMode {
    #[default]
    System,
    Light,
    Dark,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum PitchDetectorKind {
    Yin,
    IterF0,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Preferences {
    #[serde(default)]
    pub theme_mode: ThemeMode,
    pub note_name_size: u32,
    pub dot_size: f64,
    pub root_note_color: Color,
    #[serde(default = "default_scale_note_color")]
    pub scale_note_color: Color,
    pub fretboard_color: Color,
    #[serde(default = "default_dark_root_note_color")]
    pub dark_root_note_color: Color,
    #[serde(default = "default_dark_scale_note_color")]
    pub dark_scale_note_color: Color,
    #[serde(default = "default_dark_fretboard_color")]
    pub dark_fretboard_color: Color,
    #[serde(default = "default_label_color")]
    pub label_color: Color,
    #[serde(default = "default_dark_label_color")]
    pub dark_label_color: Color,
    pub fret_style: FretStyle,
    pub fret_thickness: f64,
    pub default_temperament: String,
    pub default_instrument: Option<String>,
    pub default_tuning: HashMap<(String, String), String>,
    #[serde(default)]
    pub notation_system_prefs: HashMap<String, String>,
    #[serde(default = "default_concert_hz")]
    pub concert_hz: f64,
    #[serde(default = "default_concert_octave")]
    pub concert_octave: i32,
    #[serde(default = "default_pitch_detector")]
    pub pitch_detector: PitchDetectorKind,
    /// Global default handedness. Can be overridden per instrument.
    #[serde(default)]
    pub left_handed: bool,
}

fn default_scale_note_color() -> Color {
    Color::rgb_bytes(57, 112, 217)
}
fn default_dark_root_note_color() -> Color {
    Color::rgb_bytes(167, 139, 250)
}
fn default_dark_scale_note_color() -> Color {
    Color::rgb_bytes(96, 165, 250)
}
fn default_dark_fretboard_color() -> Color {
    Color::rgb_bytes(30, 30, 46)
}
fn default_label_color() -> Color {
    Color::rgb_bytes(38, 38, 38)
}
fn default_dark_label_color() -> Color {
    Color::rgb_bytes(220, 220, 230)
}
fn default_concert_hz() -> f64 {
    440.0
}
fn default_concert_octave() -> i32 {
    4
}
fn default_pitch_detector() -> PitchDetectorKind {
    PitchDetectorKind::Yin
}

impl Preferences {
    /// Returns the four diagram colors for the given dark/light mode.
    pub fn diagram_colors(&self, dark: bool) -> DiagramColors {
        if dark {
            DiagramColors {
                root: self.dark_root_note_color,
                scale: self.dark_scale_note_color,
                board: self.dark_fretboard_color,
                label: self.dark_label_color,
            }
        } else {
            DiagramColors {
                root: self.root_note_color,
                scale: self.scale_note_color,
                board: self.fretboard_color,
                label: self.label_color,
            }
        }
    }
}

impl Default for Preferences {
    fn default() -> Self {
        Self {
            theme_mode: ThemeMode::System,
            note_name_size: 12,
            dot_size: 1.0,
            root_note_color: DiagramColors::default_light().root, // Color::rgb_bytes(51, 92, 255),
            scale_note_color: DiagramColors::default_light().scale, // Color::rgb_bytes(57, 112, 217),
            fretboard_color: DiagramColors::default_light().board,  // ::rgb_bytes(255, 255, 255),
            dark_root_note_color: Color::rgb_bytes(167, 139, 250),
            dark_scale_note_color: Color::rgb_bytes(96, 165, 250),
            dark_fretboard_color: Color::rgb_bytes(30, 30, 46),
            label_color: Color::rgb_bytes(38, 38, 38),
            dark_label_color: Color::rgb_bytes(220, 220, 230),
            fret_style: FretStyle::Solid,
            fret_thickness: 1.0,
            default_temperament: "12-TET".into(),
            default_instrument: None,
            default_tuning: HashMap::new(),
            notation_system_prefs: HashMap::new(),
            concert_hz: 440.0,
            concert_octave: 4,
            pitch_detector: PitchDetectorKind::Yin,
            left_handed: false,
        }
    }
}
