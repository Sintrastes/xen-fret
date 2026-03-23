use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub use crate::notation::{Accidental, AccidentalPosition, Natural, NotationSystem};

/// Persisted desktop window position and size (logical pixels, DPI-independent).
/// Not part of `Preferences` — tracked separately in native storage.
#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub struct WindowGeometry {
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum FretMarker {
    Single,
    Double,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Tuning {
    #[serde(default)]
    pub temperament_name: String,
    pub name: String,
    pub instrument: String,
    /// Pitch of each string in EDO steps from root
    pub string_tunings: Vec<i32>,
    pub skip_frets: u32,
    /// Neck position markers (inlays). Each entry is (fret_number, marker_type).
    #[serde(default)]
    pub fret_markers: Vec<(u32, FretMarker)>,
    /// Octave of the lowest sounding string, matching standard pitch notation
    /// (e.g. E2 → 2, G3 → 3). Used to anchor playback and mic pitch matching
    /// in the correct register. Default: 2 (guitar range).
    #[serde(default = "default_root_octave")]
    pub root_octave: i32,
}

fn default_root_octave() -> i32 { 2 }

impl Tuning {
    /// Compute the Hz of EDO step 0 (A in 12-TET) for this tuning's register.
    ///
    /// `concert_hz` / `concert_octave` define the reference pitch (e.g. 440 Hz, octave 4 for A4).
    /// `divisions` and `period` come from the temperament.
    ///
    /// In 12-TET, octave numbers increment at C (step 3 from A), so a tuning whose
    /// lowest string is at or above step 3 is in the *next* named octave relative to A.
    pub fn step0_hz(
        &self,
        concert_hz: f64,
        concert_octave: i32,
        divisions: u32,
        period: (u32, u32),
    ) -> f64 {
        let period_ratio = period.0 as f64 / period.1 as f64;
        let lowest_step = self.string_tunings.iter().min().copied().unwrap_or(0);
        let pc = lowest_step.rem_euclid(divisions as i32);
        // In 12-TET, C (step 3) starts a new octave number in standard notation.
        // If the lowest string's pitch class is >= 3, the A below it is one named-octave lower.
        let a_octave = if divisions == 12 && pc >= 3 {
            self.root_octave - 1
        } else {
            self.root_octave
        };
        concert_hz * period_ratio.powi(a_octave - concert_octave)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Scale {
    #[serde(default)]
    pub temperament_name: String,
    pub name: String,
    /// Intervals between notes (in EDO steps)
    pub intervals: Vec<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Chord {
    #[serde(default)]
    pub temperament_name: String,
    pub name: String,
    pub intervals: Vec<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Temperament {
    pub name: String,
    /// Number of equal divisions of the period (e.g. 12 for 12-TET)
    pub divisions: u32,
    /// Period as a ratio (numerator, denominator), e.g. (2, 1) for an octave
    pub period: (u32, u32),
}

impl Temperament {
    pub fn period_str(&self) -> String {
        if self.period.1 == 1 {
            format!("{}/1", self.period.0)
        } else {
            format!("{}/{}", self.period.0, self.period.1)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Instrument {
    pub name: String,
    pub instrument_type: String,
    pub temperament_name: String,
    pub num_strings: u32,
    pub num_frets: u32,
    #[serde(default)]
    pub fret_markers: Vec<(u32, FretMarker)>,
    /// Per-instrument handedness override. `None` = follow the global preference.
    #[serde(default)]
    pub left_handed: Option<bool>,
}

pub fn default_instrument_types() -> &'static [&'static str] {
    &["Guitar", "Bass Guitar", "Mandolin", "Ukulele", "Banjo", "Oud", "Koto", "Other"]
}

pub fn default_string_count(instrument_type: &str) -> u32 {
    match instrument_type {
        "Guitar" => 6,
        "Bass Guitar" => 4,
        "Mandolin" => 4,
        "Ukulele" => 4,
        "Banjo" => 5,
        "Oud" => 11,
        "Koto" => 13,
        _ => 6,
    }
}

/// Suggest a tuning for a new instrument by searching the temperament's built-in tunings
/// for a matching instrument type and string count. Falls back to a P4-based tuning.
pub fn suggest_tuning(
    tunings: &[Tuning],
    temperaments: &[Temperament],
    temp_name: &str,
    instrument_type: &str,
    num_strings: u32,
) -> Vec<i32> {
    if let Some(tu) = tunings.iter().find(|tu| {
        tu.temperament_name == temp_name
            && tu.string_tunings.len() == num_strings as usize
            && tu.instrument.to_lowercase().contains(&instrument_type.to_lowercase())
    }) {
        return tu.string_tunings.clone();
    }
    if let Some(temp) = temperaments.iter().find(|t| t.name == temp_name) {
        // Fall back to P4-based tuning
        let p4_steps = (temp.divisions as f64 * (4.0_f64 / 3.0).ln() / 2.0_f64.ln()).round() as i32;
        return (0..num_strings as i32).map(|i| i * p4_steps).collect();
    }
    // No temperament found; default to 12-TET P4 spacing (5 semitones)
    (0..num_strings as i32).map(|i| i * 5).collect()
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Color {
    pub fn to_hex(&self) -> String {
        format!("#{:02x}{:02x}{:02x}", self.r, self.g, self.b)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum FretStyle {
    Solid,
    Dashed,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Default)]
pub enum ThemeMode {
    #[default]
    System,
    Light,
    Dark,
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

fn default_scale_note_color() -> Color { Color { r: 57, g: 112, b: 217 } }
fn default_dark_root_note_color()  -> Color { Color { r: 167, g: 139, b: 250 } } // violet-400
fn default_dark_scale_note_color() -> Color { Color { r: 96,  g: 165, b: 250 } } // blue-400
fn default_dark_fretboard_color()  -> Color { Color { r: 30,  g: 30,  b: 46  } } // dark surface
fn default_label_color()      -> Color { Color { r: 38,  g: 38,  b: 38  } } // near-black
fn default_dark_label_color() -> Color { Color { r: 220, g: 220, b: 230 } } // near-white
fn default_concert_hz() -> f64 { 440.0 }
fn default_concert_octave() -> i32 { 4 }
fn default_pitch_detector() -> PitchDetectorKind { PitchDetectorKind::Yin }

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum PitchDetectorKind {
    Yin,
    IterF0,
}

impl Preferences {
    /// Returns (root_color, scale_color, fretboard_color, label_color) for the given dark flag.
    pub fn active_diagram_colors(&self, dark: bool) -> (&Color, &Color, &Color, &Color) {
        if dark {
            (&self.dark_root_note_color, &self.dark_scale_note_color, &self.dark_fretboard_color, &self.dark_label_color)
        } else {
            (&self.root_note_color, &self.scale_note_color, &self.fretboard_color, &self.label_color)
        }
    }
}

impl Default for Preferences {
    fn default() -> Self {
        Self {
            theme_mode: ThemeMode::System,
            note_name_size: 12,
            dot_size: 1.0,
            root_note_color: Color { r: 51, g: 92, b: 255 },
            scale_note_color: Color { r: 57, g: 112, b: 217 },
            fretboard_color: Color { r: 255, g: 255, b: 255 },
            dark_root_note_color: Color { r: 167, g: 139, b: 250 },
            dark_scale_note_color: Color { r: 96, g: 165, b: 250 },
            dark_fretboard_color: Color { r: 30, g: 30, b: 46 },
            label_color: Color { r: 38, g: 38, b: 38 },
            dark_label_color: Color { r: 220, g: 220, b: 230 },
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

/// Standard guitar neck inlay positions (fret number, marker type).
pub fn guitar_markers() -> Vec<(u32, FretMarker)> {
    vec![
        (3, FretMarker::Single),
        (5, FretMarker::Single),
        (7, FretMarker::Single),
        (9, FretMarker::Single),
        (12, FretMarker::Double),
        (15, FretMarker::Single),
        (17, FretMarker::Single),
        (19, FretMarker::Single),
        (21, FretMarker::Single),
        (24, FretMarker::Double),
    ]
}

struct TemperamentBundle {
    temperament: Temperament,
    notation_systems: Vec<NotationSystem>,
    tunings: Vec<Tuning>,
    scales: Vec<Scale>,
    chords: Vec<Chord>,
}

fn default_bundles() -> Vec<TemperamentBundle> {
    vec![
        TemperamentBundle {
            temperament: Temperament {
                name: "11-TET".into(),
                divisions: 11,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "11-TET".into(),
                // 11edo: 6 naturals spanning the octave, each 2 steps apart (except last gap of 1).
                // Sharp (↑1 step) and flat (↓1 step) cover the one chromatic step.
                // Letter names from the Xen Wiki "machine[6]" / Orwell approach.
                name: "Machine[6]".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0 },
                    Natural { name: "B".into(), degree: 2 },
                    Natural { name: "C".into(), degree: 4 },
                    Natural { name: "D".into(), degree: 6 },
                    Natural { name: "E".into(), degree: 8 },
                    Natural { name: "F".into(), degree: 10 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![
                Chord { temperament_name: "11-TET".into(), name: "Major".into(), intervals: vec![4, 3, 4] },
                Chord { temperament_name: "11-TET".into(), name: "Minor".into(), intervals: vec![3, 4, 4] },
            ],
            scales: vec![
                Scale { temperament_name: "11-TET".into(), name: "Orgone[7]".into(), intervals: vec![1, 2, 1, 2, 1, 2, 2] },
                Scale { temperament_name: "11-TET".into(), name: "Machine[5]".into(), intervals: vec![2, 2, 2, 2, 3] },
                Scale { temperament_name: "11-TET".into(), name: "Machine[6]".into(), intervals: vec![2, 2, 2, 2, 2, 1] },
                Scale { temperament_name: "11-TET".into(), name: "Joan heptatonic".into(), intervals: vec![1, 1, 1, 3, 1, 1, 3] },
                Scale { temperament_name: "11-TET".into(), name: "Joan pentatonic".into(), intervals: vec![1, 4, 1, 4, 1] },
            ],
            tunings: vec![
                Tuning {
                    temperament_name: "11-TET".into(),
                    name: "Wide Fourths Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    string_tunings: vec![0, 5, 10, 15, 20, 25],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "11-TET".into(),
                    name: "Major Thirds Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    string_tunings: vec![0, 4, 8, 12, 16, 20],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "11-TET".into(),
                    name: "Wide Fourths Tuning".into(),
                    instrument: "Four-String Bass Guitar".into(),
                    string_tunings: vec![0, 5, 10, 15],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "11-TET".into(),
                    name: "Major Thirds Tuning".into(),
                    instrument: "Four-String Bass Guitar".into(),
                    string_tunings: vec![0, 4, 8, 120],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
            ],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "12-TET".into(),
                divisions: 12,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "12-TET".into(),
                name: "".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0 },
                    Natural { name: "B".into(), degree: 2 },
                    Natural { name: "C".into(), degree: 3 },
                    Natural { name: "D".into(), degree: 5 },
                    Natural { name: "E".into(), degree: 7 },
                    Natural { name: "F".into(), degree: 8 },
                    Natural { name: "G".into(), degree: 10 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1, position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![
                Chord { temperament_name: "12-TET".into(), name: "Major".into(), intervals: vec![4, 3, 5] },
                Chord { temperament_name: "12-TET".into(), name: "Minor".into(), intervals: vec![3, 4, 5] },
                Chord { temperament_name: "12-TET".into(), name: "Major 7th".into(), intervals: vec![4, 3, 4, 1] },
                Chord { temperament_name: "12-TET".into(), name: "Dominant 7th".into(), intervals: vec![4, 3, 3, 2] },
                Chord { temperament_name: "12-TET".into(), name: "Minor 7th".into(), intervals: vec![3, 4, 3, 2] },
                Chord { temperament_name: "12-TET".into(), name: "MinMaj 7th".into(), intervals: vec![3, 4, 4, 1] },
            ],
            scales: vec![
                Scale { temperament_name: "12-TET".into(), name: "Ionian (Major)".into(), intervals: vec![2, 2, 1, 2, 2, 2, 1] },
                Scale { temperament_name: "12-TET".into(), name: "Mixolydian".into(), intervals: vec![2, 2, 1, 2, 2, 1, 2] },
                Scale { temperament_name: "12-TET".into(), name: "Minor".into(), intervals: vec![2, 1, 2, 2, 1, 2, 2] },
                Scale { temperament_name: "12-TET".into(), name: "Dorian".into(), intervals: vec![2, 1, 2, 2, 2, 1, 2] },
                Scale { temperament_name: "12-TET".into(), name: "diminished[8] (Octatonic)".into(), intervals: vec![2, 1, 2, 1, 2, 1, 2, 1] },
                Scale { temperament_name: "12-TET".into(), name: "Whole tone".into(), intervals: vec![2, 2, 2, 2, 2, 2] },
                Scale { temperament_name: "12-TET".into(), name: "augmented[6]".into(), intervals: vec![3, 1, 3, 1, 3, 1] },
                Scale { temperament_name: "12-TET".into(), name: "Blues".into(), intervals: vec![3, 2, 1, 1, 3, 2] },
                Scale { temperament_name: "12-TET".into(), name: "Mixolydian b6".into(), intervals: vec![2, 2, 1, 2, 1, 2, 2] },
                Scale { temperament_name: "12-TET".into(), name: "Hirojoshi".into(), intervals: vec![2, 1, 4, 1, 4] },
                Scale { temperament_name: "12-TET".into(), name: "Ryo".into(), intervals: vec![2, 2, 3, 2, 3] },
                Scale { temperament_name: "12-TET".into(), name: "Insen".into(), intervals: vec![1, 4, 2, 3, 2] },
                Scale { temperament_name: "12-TET".into(), name: "Engimatic Scale".into(), intervals: vec![1, 3, 2, 2, 2, 1, 1] },
            ],
            tunings: vec![
                Tuning {
                    temperament_name: "12-TET".into(),
                    name: "Standard Tuning".into(),
                    instrument: "Mandolin".into(),
                    // fmap (+10) $ 0 :| [7, 14, 21]
                    string_tunings: vec![10, 17, 24, 31],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 3, // G3
                },
                Tuning {
                    temperament_name: "12-TET".into(),
                    name: "Standard Tuning".into(),
                    instrument: "Ukulele".into(),
                    // fmap (+3) $ 7 :| [0, 4, 9]
                    string_tunings: vec![10, 3, 7, 12],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 4, // re-entrant G tuning; lowest sounding is C4
                },
                Tuning {
                    temperament_name: "12-TET".into(),
                    name: "Standard Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+7) $ 0 :| [5, 10, 15, 19, 24]
                    string_tunings: vec![7, 12, 17, 22, 26, 31],
                    skip_frets: 0,
                    fret_markers: guitar_markers(),
                    root_octave: 2, // E2
                },
                Tuning {
                    temperament_name: "12-TET".into(),
                    name: "Standard Tuning".into(),
                    instrument: "Four-String Bass Guitar".into(),
                    // fmap (+7) $ 0 :| [5, 10, 15]
                    string_tunings: vec![7, 12, 17, 22],
                    skip_frets: 0,
                    fret_markers: guitar_markers(),
                    root_octave: 1, // E1
                },
                Tuning {
                    temperament_name: "12-TET".into(),
                    name: "Standard Tuning".into(),
                    instrument: "Seven-String Guitar".into(),
                    // fmap (+2) $ 0 :| [5, 10, 15, 20, 14, 29]
                    string_tunings: vec![2, 7, 12, 17, 22, 16, 31],
                    skip_frets: 0,
                    fret_markers: guitar_markers(),
                    root_octave: 1, // B1
                },
                Tuning {
                    temperament_name: "12-TET".into(),
                    name: "Drop D".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+5) $ 0 :| [7, 12, 17, 21, 26]
                    string_tunings: vec![5, 12, 17, 22, 26, 31],
                    skip_frets: 0,
                    fret_markers: guitar_markers(),
                    root_octave: 2, // D2
                },
                Tuning {
                    temperament_name: "12-TET".into(),
                    name: "DADGAD".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+5) $ 0 :| [7, 12, 17, 19, 24]
                    string_tunings: vec![5, 12, 17, 22, 24, 29],
                    skip_frets: 0,
                    fret_markers: guitar_markers(),
                    root_octave: 2, // D2
                },
                Tuning {
                    temperament_name: "12-TET".into(),
                    name: "All Fourths".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+7) $ 0 :| [5, 10, 15, 20, 25]
                    string_tunings: vec![7, 12, 17, 22, 27, 32],
                    skip_frets: 0,
                    fret_markers: guitar_markers(),
                    root_octave: 2, // E2
                },
                Tuning {
                    temperament_name: "12-TET".into(),
                    name: "All Fifths".into(),
                    instrument: "Six-String Guitar".into(),
                    string_tunings: vec![0, 7, 14, 21, 28, 35],
                    skip_frets: 0,
                    fret_markers: guitar_markers(),
                    root_octave: 1, // A1
                },
            ],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "13-TET".into(),
                divisions: 13,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "13-TET".into(),
                // 13edo: 8 naturals (Oneirotonic / Dylathian notation from Xen Wiki).
                // Pattern: 2 2 1 2 2 1 2 1 = 13 steps.  Degree 0 = A (concert pitch).
                // Sharp raises by 1 step; flat lowers by 1 step.
                name: "Oneirotonic".into(),
                naturals: vec![
                    Natural { name: "J".into(), degree: 0 },
                    Natural { name: "K".into(), degree: 2 },
                    Natural { name: "L".into(), degree: 4 },
                    Natural { name: "M".into(), degree: 5 },
                    Natural { name: "N".into(), degree: 7 },
                    Natural { name: "O".into(), degree: 9 },
                    Natural { name: "P".into(), degree: 10 },
                    Natural { name: "Q".into(), degree: 12 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale {
                    temperament_name: "13-TET".into(),
                    name: "Archeotonic (Ryonian Mode)".into(),
                    intervals: vec![2, 2, 2, 2, 2, 2, 1],
                },
                Scale {
                    temperament_name: "13-TET".into(),
                    name: "Oneirotonic (Dylathian Mode)".into(),
                    intervals: vec![2, 2, 1, 2, 2, 1, 2, 1],
                },
            ],
            tunings: vec![Tuning {
                temperament_name: "13-TET".into(),
                name: "Oneirotonic Tuning".into(),
                instrument: "Six-String Guitar".into(),
                string_tunings: vec![3, 8, 14, 19, 24, 29],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "14-TET".into(),
                divisions: 14,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "14-TET".into(),
                // 14edo: 7 naturals from A=0, each 2 steps apart (whole-tone EDO character).
                // Sharp raises 1 step; flat lowers 1 step.  Up (^) and down (v) are the same.
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0 },
                    Natural { name: "B".into(), degree: 2 },
                    Natural { name: "C".into(), degree: 4 },
                    Natural { name: "D".into(), degree: 6 },
                    Natural { name: "E".into(), degree: 8 },
                    Natural { name: "F".into(), degree: 10 },
                    Natural { name: "G".into(), degree: 12 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "14-TET".into(), name: "Titanium[9]".into(), intervals: vec![2, 1, 2, 1, 2, 1, 2, 1, 2] },
                Scale { temperament_name: "14-TET".into(), name: "antipentic".into(), intervals: vec![4, 1, 4, 1, 4] },
                Scale { temperament_name: "14-TET".into(), name: "Manual".into(), intervals: vec![3, 3, 2, 3, 3] },
                Scale { temperament_name: "14-TET".into(), name: "Citric".into(), intervals: vec![3, 1, 3, 3, 1, 3] },
                Scale { temperament_name: "14-TET".into(), name: "Ekic".into(), intervals: vec![2, 2, 1, 2, 2, 2, 1, 2] },
                Scale { temperament_name: "14-TET".into(), name: "Semiquartal".into(), intervals: vec![2, 1, 2, 1, 2, 1, 2, 1, 2] },
            ],
            tunings: vec![Tuning {
                temperament_name: "14-TET".into(),
                name: "Wide Fourths Tuning".into(),
                instrument: "Six-String Guitar".into(),
                string_tunings: vec![0, 5, 10, 15, 20, 25],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "15-TET".into(),
                divisions: 15,
                period: (2, 1),
            },
            notation_systems: vec![
                NotationSystem {
                    temperament_name: "15-TET".into(),
                    // Original Greek letter notation: 8 naturals at even steps,
                    // single flat (\) covers all 15 steps.
                    name: "Greek".into(),
                    naturals: vec![
                        Natural { name: "α".into(), degree: 0  },
                        Natural { name: "β".into(), degree: 2  },
                        Natural { name: "χ".into(), degree: 4  },
                        Natural { name: "δ".into(), degree: 6  },
                        Natural { name: "ε".into(), degree: 8  },
                        Natural { name: "φ".into(), degree: 10 },
                        Natural { name: "γ".into(), degree: 12 },
                        Natural { name: "η".into(), degree: 14 },
                    ],
                    accidentals: vec![
                        Accidental { name: "\\".into(), offset: -1, position: AccidentalPosition::Postfix },
                    ],
                },
                NotationSystem {
                    temperament_name: "15-TET".into(),
                    // 15edo standard notation: 5 naturals (pentatonic backbone) or 7-note diatonic
                    // via porcupine/augmented family. Using standard letters A–G mapped to the
                    // "blackwood" / augmented structure: 5 groups of 3 steps. A sharp/flat = 1 step.
                    // Naturals follow the closest diatonic mapping (major scale 3 2 2 2 2 2 2 from wiki).
                    name: "Standard".into(),
                    naturals: vec![
                        Natural { name: "A".into(), degree: 0 },
                        Natural { name: "B".into(), degree: 3 },
                        Natural { name: "C".into(), degree: 5 },
                        Natural { name: "D".into(), degree: 7 },
                        Natural { name: "E".into(), degree: 9 },
                        Natural { name: "F".into(), degree: 11 },
                        Natural { name: "G".into(), degree: 13 },
                    ],
                    accidentals: vec![
                        Accidental { name: "\u{E262}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                        Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                    ],
                },
            ],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "15-TET".into(), name: "Augmented[6]".into(), intervals: vec![4, 1, 4, 1, 4, 1] },
                Scale { temperament_name: "15-TET".into(), name: "Triforce[6]".into(), intervals: vec![3, 2, 3, 2, 3, 2] },
                Scale { temperament_name: "15-TET".into(), name: "Porcupine[7]".into(), intervals: vec![3, 2, 2, 2, 2, 2, 2] },
                Scale { temperament_name: "15-TET".into(), name: "Orgone[7]".into(), intervals: vec![1, 3, 1, 3, 1, 3, 3] },
                Scale { temperament_name: "15-TET".into(), name: "Porcupine[8]".into(), intervals: vec![2, 1, 2, 2, 2, 2, 2, 2] },
                Scale { temperament_name: "15-TET".into(), name: "Augmented[9]".into(), intervals: vec![3, 1, 1, 3, 1, 1, 3, 1, 1] },
                Scale { temperament_name: "15-TET".into(), name: "Triforce[9]".into(), intervals: vec![2, 1, 2, 2, 1, 2, 2, 1, 2] },
                Scale { temperament_name: "15-TET".into(), name: "Blackwood[10]".into(), intervals: vec![2, 1, 2, 1, 2, 1, 2, 1, 2, 1] },
                Scale { temperament_name: "15-TET".into(), name: "Marvel double harmonic major".into(), intervals: vec![1, 4, 1, 3, 1, 4, 1] },
                Scale { temperament_name: "15-TET".into(), name: "Ptolemy diatonic, \"just\" major".into(), intervals: vec![3, 2, 1, 3, 2, 3, 1] },
                Scale { temperament_name: "15-TET".into(), name: "Ptolemy diatonic, natural minor".into(), intervals: vec![3, 1, 2, 3, 1, 3, 2] },
                Scale { temperament_name: "15-TET".into(), name: "tetrachordal major, Sa grama".into(), intervals: vec![3, 2, 1, 3, 3, 2, 1] },
                Scale { temperament_name: "15-TET".into(), name: "tetrachordal minor".into(), intervals: vec![3, 1, 2, 3, 1, 2, 3] },
                Scale { temperament_name: "15-TET".into(), name: "Porcupine bright major #7".into(), intervals: vec![3, 2, 2, 2, 2, 3, 1] },
                Scale { temperament_name: "15-TET".into(), name: "Porcupine bright major #6 #7".into(), intervals: vec![3, 2, 2, 2, 3, 2, 1] },
                Scale { temperament_name: "15-TET".into(), name: "Porcupine bright minor #2".into(), intervals: vec![3, 1, 3, 2, 2, 2, 2] },
                Scale { temperament_name: "15-TET".into(), name: "Porcupine dark minor #2".into(), intervals: vec![3, 1, 2, 3, 2, 2, 2] },
                Scale { temperament_name: "15-TET".into(), name: "Porcupine bright harmonic 11th".into(), intervals: vec![3, 2, 2, 2, 2, 1, 3] },
            ],
            tunings: vec![Tuning {
                temperament_name: "15-TET".into(),
                name: "All Fourths Tuning".into(),
                instrument: "Six-String Guitar".into(),
                string_tunings: vec![0, 5, 10, 15, 20, 25],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "16-TET".into(),
                divisions: 16,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "16-TET".into(),
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0 },
                    Natural { name: "B".into(), degree: 2 },
                    Natural { name: "C".into(), degree: 5 },
                    Natural { name: "D".into(), degree: 7 },
                    Natural { name: "E".into(), degree: 9 },
                    Natural { name: "F".into(), degree: 12 },
                    Natural { name: "G".into(), degree: 14 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: -1, position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: 1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "16-TET".into(), name: "Mavilla[5]".into(), intervals: vec![5, 2, 5, 2, 2] },
                Scale { temperament_name: "16-TET".into(), name: "Mavila[7]".into(), intervals: vec![2, 2, 2, 3, 2, 2, 3] },
                Scale { temperament_name: "16-TET".into(), name: "Mavilla[9]".into(), intervals: vec![1, 2, 2, 2, 1, 2, 2, 2, 2] },
                Scale { temperament_name: "16-TET".into(), name: "Lemba[6]".into(), intervals: vec![3, 3, 2, 3, 3, 2] },
                Scale { temperament_name: "16-TET".into(), name: "Lemba[10]".into(), intervals: vec![2, 1, 2, 1, 2, 2, 1, 2, 1, 2] },
                Scale { temperament_name: "16-TET".into(), name: "Magic[7]".into(), intervals: vec![1, 4, 1, 4, 1, 4, 1] },
                Scale { temperament_name: "16-TET".into(), name: "Magic[10]".into(), intervals: vec![1, 3, 1, 1, 3, 1, 1, 1, 3, 1] },
                Scale { temperament_name: "16-TET".into(), name: "Gorgo[5]".into(), intervals: vec![3, 3, 4, 3, 3] },
                Scale { temperament_name: "16-TET".into(), name: "Gorgo[6]".into(), intervals: vec![3, 3, 1, 3, 3, 3] },
                Scale { temperament_name: "16-TET".into(), name: "Gorgo[11]".into(), intervals: vec![1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1] },
                Scale { temperament_name: "16-TET".into(), name: "Diminished[8]".into(), intervals: vec![1, 3, 1, 3, 1, 3, 1, 3] },
            ],
            tunings: vec![
                Tuning {
                    temperament_name: "16-TET".into(),
                    name: "Wide Fourths Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+9) $ 0 :| [7, 14, 21, 28, 35]
                    string_tunings: vec![9, 16, 23, 30, 37, 44],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "16-TET".into(),
                    name: "Diminished Fourths Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+9) $ 0 :| [6, 12, 18, 24, 30]
                    string_tunings: vec![9, 15, 21, 27, 33, 39],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "16-TET".into(),
                    name: "Wide Fourths Tuning (7 String)".into(),
                    instrument: "Seven-String Guitar".into(),
                    // fmap (+9) $ 0 :| [7, 14, 21, 28, 35, 40]
                    string_tunings: vec![9, 16, 23, 30, 37, 44, 49],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "16-TET".into(),
                    name: "Diminished Fourths Tuning (7 String)".into(),
                    instrument: "Seven-String Guitar".into(),
                    // fmap (+2) $ 0 :| [6, 12, 18, 24, 30, 36]
                    string_tunings: vec![2, 8, 14, 20, 26, 32, 38],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
            ],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "17-TET".into(),
                divisions: 17,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "17-TET".into(),
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0 },
                    Natural { name: "B".into(), degree: 3 },
                    Natural { name: "C".into(), degree: 4 },
                    Natural { name: "D".into(), degree: 7 },
                    Natural { name: "E".into(), degree: 10 },
                    Natural { name: "F".into(), degree: 11 },
                    Natural { name: "G".into(), degree: 14 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 2, position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -2, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "17-TET".into(), name: "Major".into(), intervals: vec![3, 3, 3, 1, 3, 3, 1] },
                Scale { temperament_name: "17-TET".into(), name: "Maqamic[7] (bish mode)".into(), intervals: vec![2, 3, 2, 3, 2, 3, 2] },
                Scale { temperament_name: "17-TET".into(), name: "Maqamic[7] (dril mode)".into(), intervals: vec![3, 2, 3, 2, 3, 2, 2] },
                Scale { temperament_name: "17-TET".into(), name: "Maqamic[7] (fish mode)".into(), intervals: vec![2, 3, 2, 3, 2, 2, 3] },
                Scale { temperament_name: "17-TET".into(), name: "Maqamic[7] (gil mode)".into(), intervals: vec![3, 2, 3, 2, 2, 3, 2] },
                Scale { temperament_name: "17-TET".into(), name: "Maqamic[7] (jwl mode)".into(), intervals: vec![2, 3, 2, 2, 3, 2, 3] },
                Scale { temperament_name: "17-TET".into(), name: "Maqamic[7] (kleeth mode)".into(), intervals: vec![3, 2, 2, 3, 2, 3, 2] },
                Scale { temperament_name: "17-TET".into(), name: "Maqamic[7] (led mode)".into(), intervals: vec![2, 2, 3, 2, 3, 2, 3] },
                Scale { temperament_name: "17-TET".into(), name: "Maqamic[10]".into(), intervals: vec![2, 2, 2, 1, 2, 2, 1, 2, 2, 1] },
                Scale { temperament_name: "17-TET".into(), name: "Lovecraft[9]".into(), intervals: vec![3, 1, 3, 1, 3, 1, 3, 1, 1] },
                Scale { temperament_name: "17-TET".into(), name: "Squares[5]".into(), intervals: vec![5, 5, 1, 5, 1] },
                Scale { temperament_name: "17-TET".into(), name: "Squares[8]".into(), intervals: vec![1, 1, 4, 1, 4, 1, 4] },
                Scale { temperament_name: "17-TET".into(), name: "Hydra".into(), intervals: vec![3, 3, 1, 1, 2, 3, 2, 1, 1] },
                Scale { temperament_name: "17-TET".into(), name: "Springfieldian".into(), intervals: vec![3, 3, 2, 2, 3, 3, 1] },
                Scale { temperament_name: "17-TET".into(), name: "Northhaverbrookian".into(), intervals: vec![2, 3, 3, 1, 3, 3, 2] },
                Scale { temperament_name: "17-TET".into(), name: "Shelbyvillean".into(), intervals: vec![3, 3, 1, 3, 3, 2, 2] },
                Scale { temperament_name: "17-TET".into(), name: "Otonal 17".into(), intervals: vec![3, 2, 3, 2, 2, 2, 3] },
                Scale { temperament_name: "17-TET".into(), name: "Bleu[8]".into(), intervals: vec![3, 2, 2, 2, 2, 2, 2, 2] },
                Scale { temperament_name: "17-TET".into(), name: "Bleu[9]".into(), intervals: vec![1, 2, 2, 2, 2, 2, 2, 2, 2] },
                Scale { temperament_name: "17-TET".into(), name: "Machine[5]".into(), intervals: vec![5, 3, 3, 3, 3] },
                Scale { temperament_name: "17-TET".into(), name: "Machine[6]".into(), intervals: vec![2, 3, 3, 3, 3, 3] },
                Scale { temperament_name: "17-TET".into(), name: "Machine[11]".into(), intervals: vec![2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1] },
                Scale { temperament_name: "17-TET".into(), name: "Huxley[5]".into(), intervals: vec![1, 4, 4, 4, 4] },
                Scale { temperament_name: "17-TET".into(), name: "Huxley[9]".into(), intervals: vec![1, 1, 3, 1, 3, 1, 3, 1, 3] },
            ],
            tunings: vec![
                Tuning {
                    temperament_name: "17-TET".into(),
                    name: "Standard Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+10) $ 0 :| [7, 14, 21, 27, 34]
                    string_tunings: vec![10, 17, 24, 31, 37, 44],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "17-TET".into(),
                    name: "All Fourths".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+10) $ 0 :| [7, 14, 21, 28, 35]
                    string_tunings: vec![10, 17, 24, 31, 38, 45],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
            ],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "18-TET".into(),
                divisions: 18,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "18-TET".into(),
                name: "".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0 },
                    Natural { name: "B".into(), degree: 3 },
                    Natural { name: "C".into(), degree: 4 },
                    Natural { name: "D".into(), degree: 7 },
                    Natural { name: "E".into(), degree: 10 },
                    Natural { name: "F".into(), degree: 11 },
                    Natural { name: "G".into(), degree: 14 },
                    Natural { name: "H".into(), degree: 17 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 2, position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -2, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "18-TET".into(), name: "Antipentic".into(), intervals: vec![4, 4, 3, 4, 3] },
                Scale { temperament_name: "18-TET".into(), name: "Bicycle".into(), intervals: vec![4, 4, 1, 4, 4, 1] },
                Scale { temperament_name: "18-TET".into(), name: "Mavila[5]".into(), intervals: vec![2, 6, 2, 6, 2] },
                Scale { temperament_name: "18-TET".into(), name: "Malic[6]".into(), intervals: vec![2, 5, 2, 2, 5, 2] },
                Scale { temperament_name: "18-TET".into(), name: "Mish Heptatonic".into(), intervals: vec![3, 2, 3, 2, 3, 3, 2] },
                Scale { temperament_name: "18-TET".into(), name: "Smitonic".into(), intervals: vec![3, 2, 3, 2, 3, 3, 2] },
                Scale { temperament_name: "18-TET".into(), name: "Oneirotonic".into(), intervals: vec![3, 1, 3, 3, 1, 3, 3, 1] },
                Scale { temperament_name: "18-TET".into(), name: "Antiekic".into(), intervals: vec![2, 2, 3, 2, 2, 2, 3, 2] },
                Scale { temperament_name: "18-TET".into(), name: "Tcherepnin".into(), intervals: vec![4, 1, 1, 4, 1, 1, 4, 1, 1] },
                Scale { temperament_name: "18-TET".into(), name: "Taric".into(), intervals: vec![2, 2, 1, 2, 2, 2, 2, 1, 2, 2] },
            ],
            tunings: vec![Tuning {
                temperament_name: "18-TET".into(),
                name: "Wide Fourths".into(),
                instrument: "Six-String Guitar".into(),
                string_tunings: vec![0, 8, 16, 24, 32, 40],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "19-TET".into(),
                divisions: 19,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "19-TET".into(),
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0 },
                    Natural { name: "B".into(), degree: 3 },
                    Natural { name: "C".into(), degree: 5 },
                    Natural { name: "D".into(), degree: 8 },
                    Natural { name: "E".into(), degree: 11 },
                    Natural { name: "F".into(), degree: 13 },
                    Natural { name: "G".into(), degree: 16 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1, position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "19-TET".into(), name: "Ionian (Major)".into(), intervals: vec![3, 3, 2, 3, 3, 3, 2] },
                Scale { temperament_name: "19-TET".into(), name: "Sensi[5]".into(), intervals: vec![5, 5, 2, 5, 2] },
                Scale { temperament_name: "19-TET".into(), name: "Sensi[8]".into(), intervals: vec![2, 3, 2, 2, 3, 2, 2, 3] },
                Scale { temperament_name: "19-TET".into(), name: "Negri[9]".into(), intervals: vec![2, 2, 2, 2, 3, 2, 2, 2, 2] },
                Scale { temperament_name: "19-TET".into(), name: "Negri[10]".into(), intervals: vec![2, 2, 2, 2, 2, 2, 2, 2, 2, 1] },
                Scale { temperament_name: "19-TET".into(), name: "Kleismic[7]".into(), intervals: vec![1, 4, 1, 4, 1, 4, 4] },
                Scale { temperament_name: "19-TET".into(), name: "Semaphore[5]".into(), intervals: vec![4, 4, 4, 4, 3] },
                Scale { temperament_name: "19-TET".into(), name: "Semaphore[9]".into(), intervals: vec![3, 3, 1, 3, 1, 3, 1, 3, 1] },
                Scale { temperament_name: "19-TET".into(), name: "Magic[7]".into(), intervals: vec![5, 1, 5, 1, 5, 1, 1] },
                Scale { temperament_name: "19-TET".into(), name: "Magic[10]".into(), intervals: vec![4, 1, 1, 4, 1, 1, 4, 1, 1, 1] },
                Scale { temperament_name: "19-TET".into(), name: "Marvel hexatonic".into(), intervals: vec![4, 2, 5, 2, 4, 2] },
                Scale { temperament_name: "19-TET".into(), name: "deutone[6]".into(), intervals: vec![4, 3, 3, 3, 3, 3] },
                Scale { temperament_name: "19-TET".into(), name: "deutone[7]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 1] },
                Scale { temperament_name: "19-TET".into(), name: "kleismic[7]".into(), intervals: vec![4, 4, 1, 4, 1, 4, 1] },
                Scale { temperament_name: "19-TET".into(), name: "liese[5]".into(), intervals: vec![8, 1, 8, 1, 1] },
                Scale { temperament_name: "19-TET".into(), name: "liese[7]".into(), intervals: vec![7, 1, 1, 7, 1, 1, 1] },
                Scale { temperament_name: "19-TET".into(), name: "liese[9]".into(), intervals: vec![6, 1, 1, 1, 6, 1, 1, 1, 1] },
            ],
            tunings: vec![Tuning {
                temperament_name: "19-TET".into(),
                name: "Standard Tuning".into(),
                instrument: "Six-String Guitar".into(),
                // fmap (+11) $ 0 :| [8, 16, 24, 30, 38]
                string_tunings: vec![11, 19, 27, 35, 41, 49],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "20-TET".into(),
                divisions: 20,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "20-TET".into(),
                // 20edo standard diatonic notation.
                // Major scale [4,3,1,4,3,4,1]; diatonic semitone = 1, chromatic semitone = 3.
                // Naturals (from A=0): A(0), B(4), C(5), D(9), E(12), F(13), G(17).
                // Sharp = +3, flat = -3.
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 4  },
                    Natural { name: "C".into(), degree: 5  },
                    Natural { name: "D".into(), degree: 9  },
                    Natural { name: "E".into(), degree: 12 },
                    Natural { name: "F".into(), degree: 13 },
                    Natural { name: "G".into(), degree: 17 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 3,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -3, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "20-TET".into(), name: "Blackwood Major Decatonic".into(), intervals: vec![3, 1, 3, 1, 3, 1, 3, 1, 3, 1] },
                Scale { temperament_name: "20-TET".into(), name: "Blackwood Minor Decatonic".into(), intervals: vec![1, 3, 1, 3, 1, 3, 1, 3, 1, 3] },
                Scale { temperament_name: "20-TET".into(), name: "Blackwood Major Pentadecatonic".into(), intervals: vec![2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1] },
                Scale { temperament_name: "20-TET".into(), name: "Blackwood Diminished Pentadecatonic".into(), intervals: vec![1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2] },
                Scale { temperament_name: "20-TET".into(), name: "Blackwood Minor Pentadecatonic".into(), intervals: vec![1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1] },
                Scale { temperament_name: "20-TET".into(), name: "Balzano Nine-tone".into(), intervals: vec![2, 3, 2, 2, 2, 3, 2, 2, 2] },
                Scale { temperament_name: "20-TET".into(), name: "Balzano Eleven-tone".into(), intervals: vec![2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1] },
                Scale { temperament_name: "20-TET".into(), name: "Balzano Nine-tone inverse".into(), intervals: vec![2, 2, 2, 3, 2, 2, 2, 3, 2] },
                Scale { temperament_name: "20-TET".into(), name: "Balzano Eleven-tone inverse".into(), intervals: vec![1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2] },
                Scale { temperament_name: "20-TET".into(), name: "Octatonic".into(), intervals: vec![2, 3, 2, 3, 2, 3, 2, 3] },
                Scale { temperament_name: "20-TET".into(), name: "Diminished".into(), intervals: vec![3, 2, 3, 2, 3, 2, 3, 2] },
                Scale { temperament_name: "20-TET".into(), name: "Dodecatonic".into(), intervals: vec![2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1] },
                Scale { temperament_name: "20-TET".into(), name: "Major".into(), intervals: vec![4, 3, 1, 4, 3, 4, 1] },
                Scale { temperament_name: "20-TET".into(), name: "Minor".into(), intervals: vec![4, 1, 3, 4, 1, 4, 3] },
                Scale { temperament_name: "20-TET".into(), name: "Major quasi-equal Heptatonic".into(), intervals: vec![3, 3, 3, 3, 3, 3, 2] },
                Scale { temperament_name: "20-TET".into(), name: "Minor quasi-equal Heptatonic".into(), intervals: vec![3, 2, 3, 3, 3, 3, 3] },
                Scale { temperament_name: "20-TET".into(), name: "Rothenberg Generalized Diatonic".into(), intervals: vec![3, 2, 2, 2, 2, 3, 2, 2, 2] },
                Scale { temperament_name: "20-TET".into(), name: "Stearns Major".into(), intervals: vec![3, 4, 1, 4, 3, 3, 2] },
                Scale { temperament_name: "20-TET".into(), name: "score5".into(), intervals: vec![7, 2, 7, 2, 2] },
                Scale { temperament_name: "20-TET".into(), name: "Mavilla[7]".into(), intervals: vec![5, 2, 2, 5, 2, 2, 2] },
            ],
            tunings: vec![Tuning {
                temperament_name: "20-TET".into(),
                name: "Flat Forths".into(),
                instrument: "Six-String Guitar".into(),
                string_tunings: vec![0, 8, 16, 24, 32, 40],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "21-TET".into(),
                divisions: 21,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "21-TET".into(),
                // 21edo: 7 naturals using the oneirotonic/smitonic diatonic structure.
                // Oneirotonic [5L 3s] scale: [3,3,2,3,3,2,3,2] = 21 steps.
                // Using standard A–G mapped to 3-step whole tones from A=0.
                // Sharp = +1 step, flat = -1 step (chromatic semitone = 1 in 21edo).
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 3  },
                    Natural { name: "C".into(), degree: 6  },
                    Natural { name: "D".into(), degree: 9  },
                    Natural { name: "E".into(), degree: 12 },
                    Natural { name: "F".into(), degree: 15 },
                    Natural { name: "G".into(), degree: 18 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "21-TET".into(), name: "Antisinatonic (1L 9s)".into(), intervals: vec![3, 2, 2, 2, 2, 2, 2, 2, 2, 2] },
                Scale { temperament_name: "21-TET".into(), name: "Machinoid (5L 1s)".into(), intervals: vec![4, 4, 4, 4, 4, 1] },
                Scale { temperament_name: "21-TET".into(), name: "p-chro Machinoid (5L 6s)".into(), intervals: vec![3, 1, 1, 3, 1, 3, 1, 3, 1, 3, 1] },
                Scale { temperament_name: "21-TET".into(), name: "Manual (4L 1s)".into(), intervals: vec![5, 5, 5, 5, 1] },
                Scale { temperament_name: "21-TET".into(), name: "Gramitonic (4L 5s)".into(), intervals: vec![4, 1, 4, 1, 4, 1, 4, 1, 1] },
                Scale { temperament_name: "21-TET".into(), name: "Antipentic (3L 2s)".into(), intervals: vec![5, 5, 3, 5, 3] },
                Scale { temperament_name: "21-TET".into(), name: "Oneirotonic (5L 3s)".into(), intervals: vec![3, 3, 2, 3, 3, 2, 3, 2] },
                Scale { temperament_name: "21-TET".into(), name: "LH Diasem Ionian".into(), intervals: vec![3, 1, 3, 2, 3, 3, 1, 3, 2] },
                Scale { temperament_name: "21-TET".into(), name: "LH Diasem Mixo".into(), intervals: vec![3, 1, 3, 2, 3, 1, 3, 2, 3] },
                Scale { temperament_name: "21-TET".into(), name: "LH Diasem Dorian".into(), intervals: vec![1, 3, 2, 3, 3, 1, 3, 2, 3] },
                Scale { temperament_name: "21-TET".into(), name: "LH Diasem Aeolian".into(), intervals: vec![3, 2, 3, 1, 3, 2, 3, 3, 1] },
                Scale { temperament_name: "21-TET".into(), name: "LH Diasem Phrygian".into(), intervals: vec![2, 3, 3, 1, 3, 2, 3, 1, 3] },
                Scale { temperament_name: "21-TET".into(), name: "LH Diasem Lydian".into(), intervals: vec![3, 3, 1, 3, 2, 3, 1, 3, 2] },
                Scale { temperament_name: "21-TET".into(), name: "LH Diasem Darkened Dorian".into(), intervals: vec![3, 2, 3, 3, 1, 3, 2, 3, 1] },
                Scale { temperament_name: "21-TET".into(), name: "LH Diasem Brightened Aeolian".into(), intervals: vec![1, 3, 2, 3, 1, 3, 2, 3, 3] },
                Scale { temperament_name: "21-TET".into(), name: "LH Diasem Locrian".into(), intervals: vec![2, 3, 1, 3, 2, 3, 3, 1, 3] },
                Scale { temperament_name: "21-TET".into(), name: "RH Diasem Ionian".into(), intervals: vec![3, 1, 3, 2, 3, 1, 3, 3, 2] },
                Scale { temperament_name: "21-TET".into(), name: "RH Diasem Mixo".into(), intervals: vec![1, 3, 3, 2, 3, 1, 3, 2, 3] },
                Scale { temperament_name: "21-TET".into(), name: "RH Diasem Dorian".into(), intervals: vec![3, 2, 3, 1, 3, 3, 2, 3, 1] },
                Scale { temperament_name: "21-TET".into(), name: "RH Diasem Aeolian".into(), intervals: vec![3, 2, 3, 1, 3, 2, 3, 1, 3] },
                Scale { temperament_name: "21-TET".into(), name: "RH Diasem Phrygian".into(), intervals: vec![2, 3, 1, 3, 3, 2, 3, 1, 3] },
                Scale { temperament_name: "21-TET".into(), name: "RH Diasem Lydian".into(), intervals: vec![3, 1, 3, 3, 2, 3, 1, 3, 2] },
                Scale { temperament_name: "21-TET".into(), name: "RH Diasem Darkened Mixo".into(), intervals: vec![3, 3, 2, 3, 1, 3, 2, 3, 1] },
                Scale { temperament_name: "21-TET".into(), name: "RH Diasem Brightened Dorian".into(), intervals: vec![1, 3, 2, 3, 1, 3, 3, 2, 3] },
                Scale { temperament_name: "21-TET".into(), name: "RH Diasem Locrian".into(), intervals: vec![2, 3, 1, 3, 2, 3, 1, 3, 3] },
            ],
            tunings: vec![Tuning {
                temperament_name: "21-TET".into(),
                name: "Standard Tuning".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 9, 18, 27, 33, 42],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "22-TET".into(),
                divisions: 22,
                period: (2, 1),
            },
            notation_systems: vec![
                NotationSystem {
                    temperament_name: "22-TET".into(),
                    name: "Sagittal".into(),
                    naturals: vec![
                        Natural { name: "A".into(), degree: 0 },
                        Natural { name: "B".into(), degree: 4 },
                        Natural { name: "C".into(), degree: 5 },
                        Natural { name: "D".into(), degree: 9 },
                        Natural { name: "E".into(), degree: 13 },
                        Natural { name: "F".into(), degree: 14 },
                        Natural { name: "G".into(), degree: 18 },
                    ],
                    accidentals: vec![
                        Accidental { name: "\u{E302}".into(), offset: 1, position: AccidentalPosition::Postfix },
                        Accidental { name: "\u{E314}".into(), offset: 2, position: AccidentalPosition::Postfix },
                        Accidental { name: "\u{E318}".into(), offset: 3, position: AccidentalPosition::Postfix },
                    ],
                },
                NotationSystem {
                    temperament_name: "22-TET".into(),
                    name: "Standard (Meantone)".into(),
                    naturals: vec![
                        Natural { name: "A".into(), degree: 0 },
                        Natural { name: "B".into(), degree: 3 },
                        Natural { name: "C".into(), degree: 6 },
                        Natural { name: "D".into(), degree: 9 },
                        Natural { name: "E".into(), degree: 12 },
                        Natural { name: "F".into(), degree: 15 },
                        Natural { name: "G".into(), degree: 18 },
                    ],
                    // Order: # then b then x (double-sharp) gives correct canonical names
                    accidentals: vec![
                        Accidental { name: "\u{E262}".into(), offset: 1, position: AccidentalPosition::Postfix },
                        Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                        Accidental { name: "\u{E263}".into(), offset: 2, position: AccidentalPosition::Postfix },
                    ],
                },
            ],
            chords: vec![
                Chord { temperament_name: "22-TET".into(), name: "Major".into(), intervals: vec![0, 7, 6, 9] },
                Chord { temperament_name: "22-TET".into(), name: "Minor".into(), intervals: vec![0, 6, 7, 9] },
                Chord { temperament_name: "22-TET".into(), name: "SuperMajor".into(), intervals: vec![0, 8, 5, 9] },
                Chord { temperament_name: "22-TET".into(), name: "SubMinor".into(), intervals: vec![0, 5, 8, 9] },
                Chord { temperament_name: "22-TET".into(), name: "Magical".into(), intervals: vec![0, 5, 7, 9] },
                Chord { temperament_name: "22-TET".into(), name: "Tiny".into(), intervals: vec![0, 5, 5, 11] },
                Chord { temperament_name: "22-TET".into(), name: "Giant".into(), intervals: vec![0, 8, 7, 6] },
                Chord { temperament_name: "22-TET".into(), name: "Minor Seventh".into(), intervals: vec![0, 6, 7, 6, 3] },
                Chord { temperament_name: "22-TET".into(), name: "Super Seventh".into(), intervals: vec![0, 8, 5, 8, 1] },
                Chord { temperament_name: "22-TET".into(), name: "Sub Seventh".into(), intervals: vec![0, 5, 8, 5, 4] },
                Chord { temperament_name: "22-TET".into(), name: "Magical Seventh".into(), intervals: vec![0, 5, 7, 6, 4] },
                Chord { temperament_name: "22-TET".into(), name: "Major Super seventh".into(), intervals: vec![0, 8, 5, 8, 1] },
                Chord { temperament_name: "22-TET".into(), name: "Minor Sub Seventh".into(), intervals: vec![0, 5, 8, 5, 4] },
                Chord { temperament_name: "22-TET".into(), name: "Super Minor Seventh".into(), intervals: vec![0, 8, 5, 6, 3] },
                Chord { temperament_name: "22-TET".into(), name: "Sub Major Seventh".into(), intervals: vec![0, 5, 8, 6, 3] },
                Chord { temperament_name: "22-TET".into(), name: "Super Sub Seventh".into(), intervals: vec![0, 8, 5, 5, 4] },
                Chord { temperament_name: "22-TET".into(), name: "Harmonic Seventh".into(), intervals: vec![0, 7, 6, 5, 4] },
                Chord { temperament_name: "22-TET".into(), name: "Tiny seventh".into(), intervals: vec![0, 5, 5, 5, 7] },
                Chord { temperament_name: "22-TET".into(), name: "Giant Sixth".into(), intervals: vec![0, 8, 7, 5, 1] },
                Chord { temperament_name: "22-TET".into(), name: "Harmonic Minor Sixth".into(), intervals: vec![0, 6, 7, 5, 4] },
            ],
            scales: vec![
                Scale { temperament_name: "22-TET".into(), name: "Superpyth[7] (Major)".into(), intervals: vec![4, 4, 1, 4, 4, 4, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth[7] (Dorian)".into(), intervals: vec![4, 1, 4, 4, 4, 1, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth[7] (Phrygian)".into(), intervals: vec![1, 4, 4, 4, 1, 4, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth[7] (Lydian)".into(), intervals: vec![4, 4, 4, 1, 4, 4, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth[7] (Mixolydian)".into(), intervals: vec![4, 4, 1, 4, 4, 1, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth[7] (Minor)".into(), intervals: vec![4, 1, 4, 4, 1, 4, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth[7] (Locrian)".into(), intervals: vec![1, 4, 4, 1, 4, 4, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Maqam Bayati".into(), intervals: vec![3, 2, 4, 4, 1, 4, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Maqam Jiharkah".into(), intervals: vec![4, 4, 1, 4, 4, 2, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Maqam Husayni 'Ushayran".into(), intervals: vec![3, 2, 4, 3, 2, 4, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Maqam Saba".into(), intervals: vec![3, 2, 4, 4, 1, 4, 2, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Maqam Rast".into(), intervals: vec![4, 2, 3, 4, 4, 2, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Major".into(), intervals: vec![4, 3, 2, 4, 3, 4, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Dorian".into(), intervals: vec![3, 2, 4, 3, 4, 2, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Phrygian".into(), intervals: vec![2, 4, 3, 4, 2, 4, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Lydian".into(), intervals: vec![4, 3, 4, 2, 4, 3, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Mixolydian".into(), intervals: vec![3, 4, 2, 4, 3, 2, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Minor".into(), intervals: vec![4, 2, 4, 3, 2, 4, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Locrian".into(), intervals: vec![2, 4, 3, 2, 4, 3, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth Blues".into(), intervals: vec![5, 4, 1, 3, 5, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Bright Minor Blues".into(), intervals: vec![6, 3, 1, 3, 6, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Astrology[6]".into(), intervals: vec![4, 3, 4, 4, 3, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Porcupine[7]".into(), intervals: vec![3, 3, 3, 4, 3, 3, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Porcupine[8]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Orwell[5]".into(), intervals: vec![5, 5, 2, 5, 5] },
                Scale { temperament_name: "22-TET".into(), name: "Orwell[9]".into(), intervals: vec![2, 3, 2, 3, 2, 3, 2, 3, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Magic[7]".into(), intervals: vec![1, 6, 1, 6, 1, 6, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Magic[10]".into(), intervals: vec![5, 1, 1, 5, 1, 1, 5, 1, 1, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Pajara[10]".into(), intervals: vec![2, 2, 3, 2, 2, 2, 2, 3, 2, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Pentachordal Decatonic".into(), intervals: vec![2, 2, 3, 2, 2, 2, 3, 2, 2, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Hedgehog[6]".into(), intervals: vec![3, 5, 3, 3, 5, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Hedgehog[8]".into(), intervals: vec![3, 3, 3, 2, 3, 3, 3, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Astrology[10]".into(), intervals: vec![3, 1, 3, 1, 3, 3, 1, 3, 1, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Doublewide[6]".into(), intervals: vec![5, 5, 1, 5, 5, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Doublewide[10]".into(), intervals: vec![4, 1, 4, 1, 1, 4, 1, 4, 1, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Porcupine bright major #7".into(), intervals: vec![4, 3, 3, 3, 3, 4, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Porcupine bright major #6 #7".into(), intervals: vec![4, 3, 3, 3, 4, 3, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Porcupine bright minor #2".into(), intervals: vec![4, 2, 4, 3, 3, 3, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Porcupine dark minor #2".into(), intervals: vec![4, 2, 3, 4, 3, 3, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Porcupine bright harmonic 11th mode".into(), intervals: vec![4, 3, 3, 3, 3, 2, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth harmonic minor".into(), intervals: vec![4, 1, 4, 4, 1, 7, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth harmonic major".into(), intervals: vec![4, 4, 1, 4, 1, 7, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth melodic minor".into(), intervals: vec![4, 1, 4, 4, 4, 4, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Superpyth double harmonic major".into(), intervals: vec![1, 7, 1, 4, 1, 7, 1] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Harmonic Minor".into(), intervals: vec![4, 2, 3, 4, 2, 5, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Harmonic Major".into(), intervals: vec![4, 3, 2, 4, 2, 5, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Syntonic Melodic Minor".into(), intervals: vec![4, 2, 3, 4, 3, 4, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Marvel Double Harmonic Major".into(), intervals: vec![2, 5, 2, 4, 2, 5, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Blackdye".into(), intervals: vec![1, 3, 2, 3, 1, 3, 2, 3, 1, 3] },
                Scale { temperament_name: "22-TET".into(), name: "Marvel Hexatonic".into(), intervals: vec![5, 2, 6, 2, 5, 2] },
                // 11-EDO inclusions (fmap (*2))
                Scale { temperament_name: "22-TET".into(), name: "Machine[6]".into(), intervals: vec![4, 4, 4, 4, 4, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Orgone[7] (Nerevarine)".into(), intervals: vec![4, 4, 2, 4, 2, 4, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Orgone[7] (Vivecan)".into(), intervals: vec![4, 2, 4, 4, 2, 4, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Orgone[7] (Lorkhanic)".into(), intervals: vec![4, 2, 4, 2, 4, 4, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Orgone[7] (Sothic)".into(), intervals: vec![4, 2, 4, 2, 4, 2, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Orgone[7] (Kagrenacan)".into(), intervals: vec![2, 4, 4, 2, 4, 2, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Orgone[7] (Almalexian)".into(), intervals: vec![2, 4, 2, 4, 4, 2, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Orgone[7] (Dagothic)".into(), intervals: vec![2, 4, 2, 4, 2, 4, 4] },
                Scale { temperament_name: "22-TET".into(), name: "Joan Pentatonic".into(), intervals: vec![2, 8, 2, 8, 2] },
                Scale { temperament_name: "22-TET".into(), name: "Joan Heptatonic".into(), intervals: vec![2, 2, 2, 6, 2, 2, 6] },
                Scale { temperament_name: "22-TET".into(), name: "Joan Nonatonic".into(), intervals: vec![2, 2, 2, 4, 2, 2, 2, 4, 2] },
            ],
            tunings: vec![
                Tuning {
                    temperament_name: "22-TET".into(),
                    name: "Standard Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+13) $ 0 :| [9, 18, 27, 35, 44]
                    string_tunings: vec![13, 22, 31, 40, 48, 57],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "22-TET".into(),
                    name: "Drop D".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+13) $ 0 :| [5, 18, 27, 35, 44]
                    string_tunings: vec![13, 18, 31, 40, 48, 57],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "22-TET".into(),
                    name: "All Fourths Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+13) $ 0 :| [9, 18, 27, 36, 45]
                    string_tunings: vec![13, 22, 31, 40, 49, 58],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "22-TET".into(),
                    name: "Narrow Fourths Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+13) $ 0 :| [8, 16, 24, 32, 40]
                    string_tunings: vec![13, 21, 29, 37, 45, 53],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "22-TET".into(),
                    name: "Wide Fourths Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+13) $ 0 :| [10, 20, 30, 40, 50]
                    string_tunings: vec![13, 23, 33, 43, 53, 63],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
            ],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "23-TET".into(),
                divisions: 23,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "23-TET".into(),
                // 23edo: 8 naturals using the oneirotonic 5L3s structure [4,1,4,4,1,4,4,1].
                // Letters J–Q (Xen Wiki oneirotonic / Dylathian convention); A=J at degree 0.
                // Sharp = +1 step, flat = -1 step.
                name: "Oneirotonic".into(),
                naturals: vec![
                    Natural { name: "J".into(), degree: 0  },
                    Natural { name: "K".into(), degree: 4  },
                    Natural { name: "L".into(), degree: 5  },
                    Natural { name: "M".into(), degree: 9  },
                    Natural { name: "N".into(), degree: 13 },
                    Natural { name: "O".into(), degree: 14 },
                    Natural { name: "P".into(), degree: 18 },
                    Natural { name: "Q".into(), degree: 22 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "23-TET".into(), name: "3L 2s (oneiro-pentatonic)".into(), intervals: vec![5, 4, 5, 5, 4] },
                Scale { temperament_name: "23-TET".into(), name: "4L 1s (bug pentatonic)".into(), intervals: vec![5, 5, 5, 5, 3] },
                Scale { temperament_name: "23-TET".into(), name: "5L 1s (machinoid)".into(), intervals: vec![4, 4, 4, 4, 4, 3] },
                Scale { temperament_name: "23-TET".into(), name: "4L 3s (smitonic)".into(), intervals: vec![5, 1, 5, 1, 5, 1, 5] },
                Scale { temperament_name: "23-TET".into(), name: "1L 6s (antiarcheotonic)".into(), intervals: vec![3, 3, 3, 5, 3, 3, 3] },
                Scale { temperament_name: "23-TET".into(), name: "2L 5s (mavila, anti-diatonic)".into(), intervals: vec![3, 3, 4, 3, 3, 3, 4] },
                Scale { temperament_name: "23-TET".into(), name: "3L 4s (mosh)".into(), intervals: vec![2, 5, 2, 5, 2, 5, 2] },
                Scale { temperament_name: "23-TET".into(), name: "5L 3s (oneirotonic)".into(), intervals: vec![4, 1, 4, 4, 1, 4, 4, 1] },
                Scale { temperament_name: "23-TET".into(), name: "7L 1s (porcupoid)".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 2] },
                Scale { temperament_name: "23-TET".into(), name: "7L 2s (mavila superdiatonic)".into(), intervals: vec![3, 3, 3, 1, 3, 3, 3, 3, 1] },
                Scale { temperament_name: "23-TET".into(), name: "5L 4s (bug semiquartal)".into(), intervals: vec![3, 2, 3, 2, 3, 2, 3, 2, 3] },
                Scale { temperament_name: "23-TET".into(), name: "3L 7s (sephiroid)".into(), intervals: vec![3, 2, 2, 3, 2, 2, 3, 2, 2, 2] },
            ],
            tunings: vec![Tuning {
                temperament_name: "23-TET".into(),
                name: "Wide Fourths".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 10, 20, 30, 40, 50],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "24-TET".into(),
                divisions: 24,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "24-TET".into(),
                name: "Stein (half-sharps)".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0 },
                    Natural { name: "B".into(), degree: 4 },
                    Natural { name: "C".into(), degree: 6 },
                    Natural { name: "D".into(), degree: 10 },
                    Natural { name: "E".into(), degree: 14 },
                    Natural { name: "F".into(), degree: 16 },
                    Natural { name: "G".into(), degree: 20 },
                ],
                // Sharp-side accidentals first for canonical display (matching original note_names)
                accidentals: vec![
                    Accidental { name: "\u{E282}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E262}".into(),        offset: 2,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E283}".into(), offset: 3,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E284}".into(), offset: -1, position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(),        offset: -2, position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E285}".into(), offset: -3, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "24-TET".into(), name: "Ionian (Major)".into(), intervals: vec![4, 4, 2, 4, 4, 4, 2] },
                Scale { temperament_name: "24-TET".into(), name: "Anchihoye: Ethiopia".into(), intervals: vec![2, 8, 3, 6, 5] },
                Scale { temperament_name: "24-TET".into(), name: "Enharmonic Phrygian".into(), intervals: vec![8, 1, 1, 8, 4, 1, 1] },
                Scale { temperament_name: "24-TET".into(), name: "Maqam Rast".into(), intervals: vec![4, 3, 3, 4, 4, 3, 3] },
                Scale { temperament_name: "24-TET".into(), name: "Mohajira[7]".into(), intervals: vec![3, 4, 3, 4, 3, 4, 3] },
            ],
            tunings: vec![
                Tuning {
                    temperament_name: "24-TET".into(),
                    name: "Standard Tuning".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+14) $ 0 :| [10, 20, 30, 38, 48]
                    string_tunings: vec![14, 24, 34, 44, 52, 62],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "24-TET".into(),
                    name: "Drop D".into(),
                    instrument: "Six-String Guitar".into(),
                    // fmap (+12) $ 0 :| [14, 24, 34, 42, 52]
                    string_tunings: vec![12, 26, 36, 46, 54, 64],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
            ],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "25-TET".into(),
                divisions: 25,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "25-TET".into(),
                // 25edo: 7-note diatonic mapping. Fifth ≈ 15 steps.
                // Major scale [4,4,1,4,4,4,4] from Xen Wiki. Diatonic semitone = 1, chromatic = 3.
                // Naturals from A=0: A(0), B(4), C(5), D(9), E(13), F(14), G(18).
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 4  },
                    Natural { name: "C".into(), degree: 5  },
                    Natural { name: "D".into(), degree: 9  },
                    Natural { name: "E".into(), degree: 13 },
                    Natural { name: "F".into(), degree: 14 },
                    Natural { name: "G".into(), degree: 18 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 3,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -3, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "25-TET".into(), name: "Bleu[8]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 4] },
                Scale { temperament_name: "25-TET".into(), name: "Bleu[9]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 3, 1] },
                Scale { temperament_name: "25-TET".into(), name: "Luna[6]".into(), intervals: vec![4, 4, 4, 4, 4, 5] },
                Scale { temperament_name: "25-TET".into(), name: "Luna[7]".into(), intervals: vec![4, 4, 4, 4, 4, 4, 1] },
                Scale { temperament_name: "25-TET".into(), name: "Gariberttet[5]".into(), intervals: vec![6, 6, 6, 6, 1] },
                Scale { temperament_name: "25-TET".into(), name: "Gariberttet[9]".into(), intervals: vec![5, 1, 5, 1, 5, 1, 5, 1, 1] },
                Scale { temperament_name: "25-TET".into(), name: "Sixix[7]".into(), intervals: vec![3, 4, 3, 4, 3, 4, 4] },
                Scale { temperament_name: "25-TET".into(), name: "Magic[7]".into(), intervals: vec![7, 1, 7, 1, 7, 1, 1] },
                Scale { temperament_name: "25-TET".into(), name: "Magic[10]".into(), intervals: vec![6, 1, 1, 6, 1, 1, 6, 1, 1, 1] },
                Scale { temperament_name: "25-TET".into(), name: "Antipentic (3L 2s)".into(), intervals: vec![2, 7, 2, 7, 7] },
                Scale { temperament_name: "25-TET".into(), name: "Checkertonic (3L 5s)".into(), intervals: vec![2, 2, 5, 2, 2, 5, 2, 5] },
                Scale { temperament_name: "25-TET".into(), name: "Pelogic[5]".into(), intervals: vec![8, 3, 8, 3, 3] },
                Scale { temperament_name: "25-TET".into(), name: "Pelogic[7]".into(), intervals: vec![5, 3, 3, 5, 3, 3, 3] },
                Scale { temperament_name: "25-TET".into(), name: "Pelogic[9]".into(), intervals: vec![2, 3, 3, 3, 2, 3, 3, 3, 3] },
                Scale { temperament_name: "25-TET".into(), name: "Triton[5]".into(), intervals: vec![11, 1, 11, 1, 1] },
                Scale { temperament_name: "25-TET".into(), name: "Triton[7]".into(), intervals: vec![10, 1, 1, 10, 1, 1, 1] },
                Scale { temperament_name: "25-TET".into(), name: "Triton[9]".into(), intervals: vec![9, 1, 1, 1, 9, 1, 1, 1, 1] },
            ],
            tunings: vec![Tuning {
                temperament_name: "25-TET".into(),
                name: "Standard Tuning".into(),
                instrument: "Six-String Guitar".into(),
                // E2-A2-D3-G3-B3-E4: P4=10, P4=10, P4=10, M3=8, P4=10
                // 25-TET P4 ≈ 10 steps (10.38), M3 ≈ 8 steps (8.05) — excellent 5/4 approx at 384¢
                string_tunings: vec![0, 10, 20, 30, 38, 48],
                skip_frets: 0,
                fret_markers: guitar_markers(),
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "26-TET".into(),
                divisions: 26,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "26-TET".into(),
                // 26edo flattone meantone notation.  Fifth = 15 steps.
                // Whole tone = 4 steps, diatonic semitone = 3 steps, chromatic semitone = 1 step.
                // Naturals from A=0: A(0), B(4), C(7), D(11), E(15), F(18), G(22).
                // Sharp = +1, flat = -1 (note: in flattone sharps < diatonic semitones).
                name: "Flattone".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 4  },
                    Natural { name: "C".into(), degree: 7  },
                    Natural { name: "D".into(), degree: 11 },
                    Natural { name: "E".into(), degree: 15 },
                    Natural { name: "F".into(), degree: 18 },
                    Natural { name: "G".into(), degree: 22 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "26-TET".into(), name: "Flattone".into(), intervals: vec![4, 4, 4, 3, 4, 4, 3] },
                Scale { temperament_name: "26-TET".into(), name: "Orgone".into(), intervals: vec![5, 5, 2, 5, 2, 5, 2] },
                Scale { temperament_name: "26-TET".into(), name: "Lemba".into(), intervals: vec![5, 5, 3, 5, 5, 3] },
            ],
            tunings: vec![Tuning {
                temperament_name: "26-TET".into(),
                name: "All Fourths".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 11, 22, 33, 44, 55],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "27-TET".into(),
                divisions: 27,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "27-TET".into(),
                // 27edo superpyth notation.  Fifth = 16 steps.
                // Superpyth[7] scale: [1,5,5,1,5,5,5]. Diatonic semitone = 1, chromatic semitone = 4.
                // Naturals from A=0: A(0), B(5), C(6), D(11), E(16), F(17), G(22).
                // Sharp = +4, flat = -4.
                name: "Superpyth".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 5  },
                    Natural { name: "C".into(), degree: 6  },
                    Natural { name: "D".into(), degree: 11 },
                    Natural { name: "E".into(), degree: 16 },
                    Natural { name: "F".into(), degree: 17 },
                    Natural { name: "G".into(), degree: 22 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 4,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -4, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "27-TET".into(), name: "Tetracot[6]".into(), intervals: vec![4, 4, 4, 4, 4, 7] },
                Scale { temperament_name: "27-TET".into(), name: "Tetractot[7]".into(), intervals: vec![4, 4, 4, 4, 4, 4, 3] },
                Scale { temperament_name: "27-TET".into(), name: "Machine[5]".into(), intervals: vec![5, 5, 5, 5, 7] },
                Scale { temperament_name: "27-TET".into(), name: "Machine[6]".into(), intervals: vec![5, 5, 5, 5, 5, 2] },
                Scale { temperament_name: "27-TET".into(), name: "Myna[7]".into(), intervals: vec![1, 6, 1, 6, 1, 6, 6] },
                Scale { temperament_name: "27-TET".into(), name: "Beatles[7]".into(), intervals: vec![5, 3, 5, 3, 5, 3, 3] },
                Scale { temperament_name: "27-TET".into(), name: "Beatles[10]".into(), intervals: vec![2, 3, 3, 2, 3, 3, 2, 3, 3, 3] },
                Scale { temperament_name: "27-TET".into(), name: "Sensi[5]".into(), intervals: vec![3, 7, 3, 7, 7] },
                Scale { temperament_name: "27-TET".into(), name: "Sensi[8]".into(), intervals: vec![3, 3, 4, 3, 3, 4, 3, 4] },
                Scale { temperament_name: "27-TET".into(), name: "Superpyth[7]".into(), intervals: vec![1, 5, 5, 1, 5, 5, 5] },
                Scale { temperament_name: "27-TET".into(), name: "Fervor[5]".into(), intervals: vec![12, 1, 12, 1, 1] },
                Scale { temperament_name: "27-TET".into(), name: "Fervor[7]".into(), intervals: vec![11, 1, 1, 11, 1, 1, 1] },
                Scale { temperament_name: "27-TET".into(), name: "Fervor[9]".into(), intervals: vec![10, 1, 1, 1, 10, 1, 1, 1, 1] },
            ],
            tunings: vec![Tuning {
                temperament_name: "27-TET".into(),
                name: "Standard Tuning".into(),
                instrument: "Six-String Guitar".into(),
                // E2-A2-D3-G3-B3-E4: P4=11, P4=11, P4=11, M3≈9, P4=11
                // 27-TET P4 ≈ 11 steps (11.21), M3 ≈ 9 steps (400¢ vs 386¢ target)
                // Note: superpyth diatonic G→B = 10 steps (444¢ ≈ 9/7); 9 steps is a
                // better 5/4 approximation for standard guitar tuning purposes.
                string_tunings: vec![0, 11, 22, 33, 42, 53],
                skip_frets: 0,
                fret_markers: guitar_markers(),
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "28-TET".into(),
                divisions: 28,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "28-TET".into(),
                // 28edo standard diatonic notation.
                // "Diatonic Major" scale: [5,4,3,4,5,5,2]. Diatonic semitone = 2, chromatic semitone = 2.
                // Naturals from A=0: A(0), B(5), C(9), D(12), E(16), F(21), G(26).
                // Sharp = +2, flat = -2.
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 5  },
                    Natural { name: "C".into(), degree: 9  },
                    Natural { name: "D".into(), degree: 12 },
                    Natural { name: "E".into(), degree: 16 },
                    Natural { name: "F".into(), degree: 21 },
                    Natural { name: "G".into(), degree: 26 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 2,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -2, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "28-TET".into(), name: "Negri [9]".into(), intervals: vec![3, 3, 3, 3, 4, 3, 3, 3, 3] },
                Scale { temperament_name: "28-TET".into(), name: "Negri [10]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 3, 3, 1] },
                Scale { temperament_name: "28-TET".into(), name: "Diatonic Major [7]".into(), intervals: vec![5, 4, 3, 4, 5, 5, 2] },
                Scale { temperament_name: "28-TET".into(), name: "Diatonic Minor [7]".into(), intervals: vec![5, 2, 5, 4, 3, 4, 5] },
                Scale { temperament_name: "28-TET".into(), name: "Diatonic Naive Major [7]".into(), intervals: vec![4, 5, 3, 4, 5, 4, 3] },
                Scale { temperament_name: "28-TET".into(), name: "Diatonic Naive Minor [7]".into(), intervals: vec![4, 3, 5, 4, 3, 4, 5] },
                Scale { temperament_name: "28-TET".into(), name: "Harmonic Minor [7]".into(), intervals: vec![5, 2, 5, 4, 3, 7, 2] },
                Scale { temperament_name: "28-TET".into(), name: "Harmonic Major [7]".into(), intervals: vec![5, 4, 3, 4, 3, 7, 2] },
                Scale { temperament_name: "28-TET".into(), name: "Diasem (Right-handed)".into(), intervals: vec![4, 1, 4, 4, 3, 4, 1, 4, 3] },
                Scale { temperament_name: "28-TET".into(), name: "Diasem (Left-handed)".into(), intervals: vec![4, 4, 1, 4, 3, 4, 1, 4, 3] },
                Scale { temperament_name: "28-TET".into(), name: "Oneirotonic [5]".into(), intervals: vec![6, 5, 6, 5, 6] },
                Scale { temperament_name: "28-TET".into(), name: "Oneirotonic [8]".into(), intervals: vec![5, 5, 1, 5, 5, 1, 5, 1] },
            ],
            tunings: vec![
                Tuning {
                    temperament_name: "28-TET".into(),
                    name: "Wide Fourths".into(),
                    instrument: "Six String Guitar".into(),
                    string_tunings: vec![0, 12, 24, 36, 48, 69],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
                Tuning {
                    temperament_name: "28-TET".into(),
                    name: "Narrow Fourths".into(),
                    instrument: "Six String Guitar".into(),
                    string_tunings: vec![0, 11, 22, 33, 44, 55],
                    skip_frets: 0,
                    fret_markers: vec![],
                    root_octave: 2,
                },
            ],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "29-TET".into(),
                divisions: 29,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "29-TET".into(),
                // 29edo meantone notation.  Fifth = 17 steps.
                // Whole tone = 5 steps, diatonic semitone = 2 steps, chromatic semitone = 3 steps.
                // Naturals from A=0: A(0), B(5), C(7), D(12), E(17), F(19), G(24).
                // Sharp = +3, flat = -3.
                name: "Meantone".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 5  },
                    Natural { name: "C".into(), degree: 7  },
                    Natural { name: "D".into(), degree: 12 },
                    Natural { name: "E".into(), degree: 17 },
                    Natural { name: "F".into(), degree: 19 },
                    Natural { name: "G".into(), degree: 24 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 3,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -3, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "29-TET".into(), name: "Nicetone".into(), intervals: vec![5, 4, 3, 5, 4, 5, 3] },
                Scale { temperament_name: "29-TET".into(), name: "Porcupine[7]".into(), intervals: vec![4, 4, 4, 4, 4, 4, 5] },
                Scale { temperament_name: "29-TET".into(), name: "Porcupine[8]".into(), intervals: vec![4, 4, 4, 4, 4, 4, 4, 1] },
                Scale { temperament_name: "29-TET".into(), name: "Negri[9]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 3, 5] },
                Scale { temperament_name: "29-TET".into(), name: "Negri[10]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 3, 3, 2] },
                Scale { temperament_name: "29-TET".into(), name: "Semaphore[5]".into(), intervals: vec![5, 6, 6, 6, 6] },
                Scale { temperament_name: "29-TET".into(), name: "Semaphore[9]".into(), intervals: vec![5, 5, 1, 5, 1, 5, 1, 5, 1] },
                Scale { temperament_name: "29-TET".into(), name: "Leapfrog[7]".into(), intervals: vec![5, 5, 5, 2, 5, 5, 2] },
            ],
            tunings: vec![Tuning {
                temperament_name: "29-TET".into(),
                name: "Standard Tuning".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 12, 24, 36, 46, 58],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "30-TET".into(),
                divisions: 30,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "30-TET".into(),
                // 30edo standard diatonic notation (meantone).
                // Mavila[7]: [5,4,4,5,4,4,4]. Naturals from A=0: A(0), B(5), C(9), D(13), E(18), F(22), G(26).
                // Sharp = +1 (chromatic semitone in 30edo), flat = -1.
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 5  },
                    Natural { name: "C".into(), degree: 9  },
                    Natural { name: "D".into(), degree: 13 },
                    Natural { name: "E".into(), degree: 18 },
                    Natural { name: "F".into(), degree: 22 },
                    Natural { name: "G".into(), degree: 26 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "30-TET".into(), name: "Lovecraft[5]".into(), intervals: vec![7, 7, 7, 7, 2] },
                Scale { temperament_name: "30-TET".into(), name: "Lovecraft[9]".into(), intervals: vec![5, 2, 5, 2, 5, 2, 5, 2, 2] },
                Scale { temperament_name: "30-TET".into(), name: "Sensi[5]".into(), intervals: vec![8, 3, 8, 3, 8] },
                Scale { temperament_name: "30-TET".into(), name: "Sensi[8]".into(), intervals: vec![5, 3, 3, 5, 3, 3, 5, 3] },
                Scale { temperament_name: "30-TET".into(), name: "Mavila[5]".into(), intervals: vec![9, 4, 9, 4, 4] },
                Scale { temperament_name: "30-TET".into(), name: "Mavila[7]".into(), intervals: vec![5, 4, 4, 5, 4, 4, 4] },
                Scale { temperament_name: "30-TET".into(), name: "Mavila[9]".into(), intervals: vec![4, 4, 4, 4, 1, 4, 4, 4, 1] },
            ],
            tunings: vec![Tuning {
                temperament_name: "30-TET".into(),
                name: "Narrow Fourths".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 12, 24, 36, 48, 60],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "31-TET".into(),
                divisions: 31,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "31-TET".into(),
                // 31edo meantone notation (Xen Wiki standard).
                // Same 7 naturals as 12-TET but scaled to 31 steps.
                // Fifth = 18 steps.  Whole tone = 5 steps, diatonic semitone = 3, chromatic semitone = 2.
                // A(0), B(5), C(8), D(13), E(18), F(21), G(26).
                // Sharp = +2 steps, flat = -2 steps.  Double-sharp = +4, double-flat = -4.
                name: "Meantone".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 5  },
                    Natural { name: "C".into(), degree: 8  },
                    Natural { name: "D".into(), degree: 13 },
                    Natural { name: "E".into(), degree: 18 },
                    Natural { name: "F".into(), degree: 21 },
                    Natural { name: "G".into(), degree: 26 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 2,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -2, position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E263}".into(), offset: 4,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E264}".into(), offset: -4, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "31-TET".into(), name: "Miracle[5]".into(), intervals: vec![3, 3, 3, 3, 19] },
                Scale { temperament_name: "31-TET".into(), name: "Nusecond[5]".into(), intervals: vec![4, 4, 4, 4, 15] },
                Scale { temperament_name: "31-TET".into(), name: "Hemithirds[5]".into(), intervals: vec![5, 5, 5, 5, 11] },
                Scale { temperament_name: "31-TET".into(), name: "Mothra[5]".into(), intervals: vec![6, 6, 6, 6, 7] },
                Scale { temperament_name: "31-TET".into(), name: "Orwell[5]".into(), intervals: vec![7, 7, 7, 7, 3] },
                Scale { temperament_name: "31-TET".into(), name: "Squares[5]".into(), intervals: vec![2, 9, 2, 9, 9] },
                Scale { temperament_name: "31-TET".into(), name: "Semisept[5]".into(), intervals: vec![5, 7, 5, 7, 7] },
                Scale { temperament_name: "31-TET".into(), name: "Meantone[5]".into(), intervals: vec![8, 5, 8, 5, 5] },
                Scale { temperament_name: "31-TET".into(), name: "Casablanca[5]".into(), intervals: vec![11, 3, 11, 3, 3] },
                Scale { temperament_name: "31-TET".into(), name: "Tritonic[5]".into(), intervals: vec![14, 1, 14, 1, 1] },
                Scale { temperament_name: "31-TET".into(), name: "Miracle[6]".into(), intervals: vec![3, 3, 3, 3, 3, 16] },
                Scale { temperament_name: "31-TET".into(), name: "Nusecond[6]".into(), intervals: vec![4, 4, 4, 4, 4, 11] },
                Scale { temperament_name: "31-TET".into(), name: "Hemithirds[6]".into(), intervals: vec![5, 5, 5, 5, 5, 6] },
                Scale { temperament_name: "31-TET".into(), name: "Mothra[6]".into(), intervals: vec![6, 6, 6, 6, 6, 1] },
                Scale { temperament_name: "31-TET".into(), name: "Miracle[7]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 13] },
                Scale { temperament_name: "31-TET".into(), name: "Nusecond[7]".into(), intervals: vec![4, 4, 4, 4, 4, 4, 7] },
                Scale { temperament_name: "31-TET".into(), name: "Hemithirds[7]".into(), intervals: vec![5, 5, 5, 5, 5, 5, 1] },
                Scale { temperament_name: "31-TET".into(), name: "Myna[7]".into(), intervals: vec![1, 7, 1, 7, 1, 7, 7] },
                Scale { temperament_name: "31-TET".into(), name: "Mohajira[7]".into(), intervals: vec![5, 4, 5, 4, 5, 4, 4] },
                Scale { temperament_name: "31-TET".into(), name: "Würschmidt[7]".into(), intervals: vec![9, 1, 9, 1, 9, 1, 1] },
                Scale { temperament_name: "31-TET".into(), name: "Meantone[7]".into(), intervals: vec![3, 5, 5, 3, 5, 5, 5] },
                Scale { temperament_name: "31-TET".into(), name: "Casablanca[7]".into(), intervals: vec![8, 3, 3, 8, 3, 3, 3] },
                Scale { temperament_name: "31-TET".into(), name: "Tritonic[7]".into(), intervals: vec![13, 1, 1, 13, 1, 1, 1] },
                Scale { temperament_name: "31-TET".into(), name: "Miracle[8]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 10] },
                Scale { temperament_name: "31-TET".into(), name: "Nusecond[8]".into(), intervals: vec![4, 4, 4, 4, 4, 4, 4, 3] },
                Scale { temperament_name: "31-TET".into(), name: "Squares[8]".into(), intervals: vec![2, 2, 7, 2, 2, 7, 2, 7] },
                Scale { temperament_name: "31-TET".into(), name: "Semisept[8]".into(), intervals: vec![5, 5, 2, 5, 5, 2, 5, 2] },
                Scale { temperament_name: "31-TET".into(), name: "Miracle[9]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 3, 7] },
                Scale { temperament_name: "31-TET".into(), name: "Orwell[9]".into(), intervals: vec![4, 3, 4, 3, 4, 3, 4, 3, 3] },
                Scale { temperament_name: "31-TET".into(), name: "Casablanca[9]".into(), intervals: vec![5, 3, 3, 3, 5, 3, 3, 3, 3] },
            ],
            tunings: vec![Tuning {
                temperament_name: "31-TET".into(),
                name: "Standard Tuning".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 13, 26, 39, 49, 62],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "32-TET".into(),
                divisions: 32,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "32-TET".into(),
                // 32edo pajara notation.
                // Pajara[7] scale: [6,6,6,1,6,6,1]. Diatonic semitone = 1, apotome = 5.
                // Naturals from A=0: A(0), B(6), C(12), D(18), E(19), F(25), G(31).
                // Sharp = +5, flat = -5.
                name: "Pajara".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 6  },
                    Natural { name: "C".into(), degree: 12 },
                    Natural { name: "D".into(), degree: 18 },
                    Natural { name: "E".into(), degree: 19 },
                    Natural { name: "F".into(), degree: 25 },
                    Natural { name: "G".into(), degree: 31 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 5,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -5, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "32-TET".into(), name: "blackdye".into(), intervals: vec![1, 5, 2, 5, 1, 5, 2, 5, 1, 5] },
                Scale { temperament_name: "32-TET".into(), name: "Sixix[7]".into(), intervals: vec![4, 5, 4, 5, 4, 5, 5] },
                Scale { temperament_name: "32-TET".into(), name: "Pajara[5]".into(), intervals: vec![6, 6, 7, 6, 7] },
                Scale { temperament_name: "32-TET".into(), name: "Pajara[7]".into(), intervals: vec![6, 6, 6, 1, 6, 6, 1] },
                Scale { temperament_name: "32-TET".into(), name: "Pentic".into(), intervals: vec![4, 4, 10, 4, 10] },
                Scale { temperament_name: "32-TET".into(), name: "Antidiatonic".into(), intervals: vec![4, 4, 4, 6, 4, 4, 6] },
            ],
            tunings: vec![Tuning {
                temperament_name: "32-TET".into(),
                name: "Wide Fourths".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 14, 28, 42, 56, 70],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "33-TET".into(),
                divisions: 33,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "33-TET".into(),
                // 33edo diatonic notation.
                // 5L2s scale [5,5,5,4,5,5,4]. Diatonic semitone = 4, chromatic semitone = 1.
                // Naturals from A=0: A(0), B(5), C(10), D(15), E(19), F(24), G(29).
                // Sharp = +1, flat = -1.
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 5  },
                    Natural { name: "C".into(), degree: 10 },
                    Natural { name: "D".into(), degree: 15 },
                    Natural { name: "E".into(), degree: 19 },
                    Natural { name: "F".into(), degree: 24 },
                    Natural { name: "G".into(), degree: 29 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "33-TET".into(), name: "1L 4s".into(), intervals: vec![6, 6, 6, 6, 9] },
                Scale { temperament_name: "33-TET".into(), name: "4L 1s".into(), intervals: vec![7, 7, 7, 7, 5] },
                Scale { temperament_name: "33-TET".into(), name: "3L 2s".into(), intervals: vec![3, 9, 3, 9, 9] },
                Scale { temperament_name: "33-TET".into(), name: "4L 1s".into(), intervals: vec![5, 7, 7, 7, 7] },
                Scale { temperament_name: "33-TET".into(), name: "1L 5s".into(), intervals: vec![5, 5, 5, 5, 5, 8] },
                Scale { temperament_name: "33-TET".into(), name: "5L 1s".into(), intervals: vec![6, 6, 6, 6, 6, 3] },
                Scale { temperament_name: "33-TET".into(), name: "5L 2s".into(), intervals: vec![5, 5, 5, 4, 5, 5, 4] },
                Scale { temperament_name: "33-TET".into(), name: "4L 3s".into(), intervals: vec![6, 6, 3, 6, 3, 6, 3] },
                Scale { temperament_name: "33-TET".into(), name: "3L 5s".into(), intervals: vec![3, 3, 6, 3, 3, 6, 3, 6] },
                Scale { temperament_name: "33-TET".into(), name: "5L 3s".into(), intervals: vec![6, 6, 1, 6, 6, 1, 6, 1] },
            ],
            tunings: vec![Tuning {
                temperament_name: "33-TET".into(),
                name: "All Fourths".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 14, 28, 42, 56, 70],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "34-TET".into(),
                divisions: 34,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "34-TET".into(),
                // 34edo meantone notation.  Fifth = 20 steps.
                // Whole tone = 6 steps, diatonic semitone = 2 steps, chromatic semitone = 4 steps.
                // Naturals from A=0: A(0), B(6), C(8), D(14), E(20), F(22), G(28).
                // Sharp = +4, flat = -4.
                name: "Meantone".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 6  },
                    Natural { name: "C".into(), degree: 8  },
                    Natural { name: "D".into(), degree: 14 },
                    Natural { name: "E".into(), degree: 20 },
                    Natural { name: "F".into(), degree: 22 },
                    Natural { name: "G".into(), degree: 28 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 4,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -4, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "34-TET".into(), name: "Tetracot[5]".into(), intervals: vec![5, 5, 5, 5, 14] },
                Scale { temperament_name: "34-TET".into(), name: "Tetracot[6]".into(), intervals: vec![5, 5, 5, 5, 5, 9] },
                Scale { temperament_name: "34-TET".into(), name: "Tetracot[7]".into(), intervals: vec![5, 5, 5, 5, 5, 5, 4] },
                Scale { temperament_name: "34-TET".into(), name: "Immunity[5]".into(), intervals: vec![7, 7, 7, 7, 6] },
                Scale { temperament_name: "34-TET".into(), name: "Immunity[9]".into(), intervals: vec![1, 6, 1, 6, 1, 6, 1, 6, 6] },
                Scale { temperament_name: "34-TET".into(), name: "Hanson[7]".into(), intervals: vec![2, 7, 2, 7, 2, 7, 7] },
                Scale { temperament_name: "34-TET".into(), name: "Petrtri[5]".into(), intervals: vec![5, 8, 5, 8, 8] },
                Scale { temperament_name: "34-TET".into(), name: "Petrtri[8]".into(), intervals: vec![5, 5, 3, 5, 5, 3, 5, 3] },
                Scale { temperament_name: "34-TET".into(), name: "Mabila[5]".into(), intervals: vec![11, 4, 11, 4, 4] },
                Scale { temperament_name: "34-TET".into(), name: "Mabila[7]".into(), intervals: vec![7, 4, 4, 7, 4, 4, 4] },
            ],
            tunings: vec![Tuning {
                temperament_name: "34-TET".into(),
                name: "Standard Tuning".into(),
                instrument: "Six-String Guitar".into(),
                // E2-A2-D3-G3-B3-E4: P4=14, P4=14, P4=14, M3≈11, P4=14
                // 34-TET P4 = 14 steps (494¢), M3 = 11 steps (388¢ ≈ 5/4 = 386¢)
                // Note: diatonic G→B = 12 steps (423¢ ≈ 9/7); 11 steps is the
                // better 5/4 approximation for standard guitar tuning purposes.
                string_tunings: vec![0, 14, 28, 42, 53, 67],
                skip_frets: 0,
                fret_markers: guitar_markers(),
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "35-TET".into(),
                divisions: 35,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "35-TET".into(),
                // 35edo ups-and-downs notation.  Fifth = 20 steps (close to 34edo meantone).
                // Whole tone = 6 steps, diatonic semitone = 2, chromatic semitone = 4.
                // Naturals from A=0: A(0), B(6), C(8), D(14), E(20), F(22), G(28).
                // Up (^) = +1 step, down (v) = -1 step; sharp = +4, flat = -4.
                name: "Ups and Downs".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 6  },
                    Natural { name: "C".into(), degree: 8  },
                    Natural { name: "D".into(), degree: 14 },
                    Natural { name: "E".into(), degree: 20 },
                    Natural { name: "F".into(), degree: 22 },
                    Natural { name: "G".into(), degree: 28 },
                ],
                accidentals: vec![
                    Accidental { name: "^".into(),          offset: 1,  position: AccidentalPosition::Prefix },
                    Accidental { name: "v".into(),          offset: -1, position: AccidentalPosition::Prefix },
                    Accidental { name: "\u{E262}".into(),   offset: 4,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(),   offset: -4, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "35-TET".into(), name: "Secund[9]".into(), intervals: vec![4, 4, 4, 4, 4, 4, 4, 4, 3] },
                Scale { temperament_name: "35-TET".into(), name: "Ripple[10]".into(), intervals: vec![3, 3, 3, 3, 3, 3, 3, 3, 3, 8] },
                Scale { temperament_name: "35-TET".into(), name: "Baldy[5]".into(), intervals: vec![6, 6, 6, 6, 11] },
                Scale { temperament_name: "35-TET".into(), name: "Baldy[6]".into(), intervals: vec![6, 6, 6, 6, 6, 5] },
                Scale { temperament_name: "35-TET".into(), name: "Baldy[11]".into(), intervals: vec![1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 5] },
                Scale { temperament_name: "35-TET".into(), name: "Orwell[5]".into(), intervals: vec![8, 8, 8, 8, 3] },
                Scale { temperament_name: "35-TET".into(), name: "Orwell[9]".into(), intervals: vec![5, 3, 5, 3, 5, 3, 5, 3, 3] },
                Scale { temperament_name: "35-TET".into(), name: "Myna[7]".into(), intervals: vec![1, 8, 1, 8, 1, 8, 8] },
                Scale { temperament_name: "35-TET".into(), name: "Myna[11]".into(), intervals: vec![1, 1, 7, 1, 1, 7, 1, 1, 7, 1, 7] },
                Scale { temperament_name: "35-TET".into(), name: "Muggles[7]".into(), intervals: vec![9, 2, 9, 2, 9, 2, 2] },
                Scale { temperament_name: "35-TET".into(), name: "Muggles[10]".into(), intervals: vec![7, 2, 2, 7, 2, 2, 7, 2, 2, 2] },
                Scale { temperament_name: "35-TET".into(), name: "Roman[5]".into(), intervals: vec![1, 11, 1, 11, 11] },
                Scale { temperament_name: "35-TET".into(), name: "Roman[8]".into(), intervals: vec![1, 1, 10, 1, 1, 10, 1, 10] },
                Scale { temperament_name: "35-TET".into(), name: "Sensi[5]".into(), intervals: vec![4, 9, 4, 9, 9] },
                Scale { temperament_name: "35-TET".into(), name: "Sensi[8]".into(), intervals: vec![4, 4, 5, 4, 4, 5, 4, 5] },
                Scale { temperament_name: "35-TET".into(), name: "Sensi[11]".into(), intervals: vec![4, 4, 4, 1, 4, 4, 4, 1, 4, 4, 1] },
            ],
            tunings: vec![Tuning {
                temperament_name: "35-TET".into(),
                name: "Wide Fourths".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 15, 30, 45, 60, 75],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "36-TET".into(),
                divisions: 36,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "36-TET".into(),
                // 36edo standard diatonic notation (12-TET × 3 = every third step matches 12-TET).
                // Whole tone = 6 steps, diatonic semitone = 3, chromatic semitone = 3.
                // Naturals from A=0: A(0), B(6), C(9), D(15), E(21), F(24), G(30).
                // Sharp = +3, flat = -3.
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 6  },
                    Natural { name: "C".into(), degree: 9  },
                    Natural { name: "D".into(), degree: 15 },
                    Natural { name: "E".into(), degree: 21 },
                    Natural { name: "F".into(), degree: 24 },
                    Natural { name: "G".into(), degree: 30 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 3,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(), offset: -3, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "36-TET".into(), name: "Pentatonic".into(), intervals: vec![6, 6, 9, 6, 9] },
                Scale { temperament_name: "36-TET".into(), name: "Lydian".into(), intervals: vec![6, 6, 6, 3, 6, 6, 3] },
            ],
            tunings: vec![Tuning {
                temperament_name: "36-TET".into(),
                name: "Standard Tuning".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 15, 30, 45, 57, 72],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "41-TET".into(),
                divisions: 41,
                period: (2, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "41-TET".into(),
                // 41edo ups-and-downs notation (Kite Giedraitis / Xen Wiki standard).
                // Fifth = 24 steps.  Whole tone = 7 steps, diatonic semitone = 3 steps, chromatic semitone = 4 steps.
                // Up (^) = +1 step, down (v) = -1 step; sharp = +4, flat = -4.
                // Naturals: A(0), B(7), C(10), D(17), E(24), F(27), G(34).
                name: "Ups and Downs".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0  },
                    Natural { name: "B".into(), degree: 7  },
                    Natural { name: "C".into(), degree: 10 },
                    Natural { name: "D".into(), degree: 17 },
                    Natural { name: "E".into(), degree: 24 },
                    Natural { name: "F".into(), degree: 27 },
                    Natural { name: "G".into(), degree: 34 },
                ],
                accidentals: vec![
                    Accidental { name: "^".into(),          offset: 1,  position: AccidentalPosition::Prefix },
                    Accidental { name: "v".into(),          offset: -1, position: AccidentalPosition::Prefix },
                    Accidental { name: "\u{E262}".into(),   offset: 4,  position: AccidentalPosition::Postfix },
                    Accidental { name: "\u{E260}".into(),   offset: -4, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "41-TET".into(), name: "Down Lydian".into(), intervals: vec![7, 6, 7, 4, 7, 6, 4] },
                Scale { temperament_name: "41-TET".into(), name: "Down Major".into(), intervals: vec![7, 6, 4, 7, 6, 7, 4] },
                Scale { temperament_name: "41-TET".into(), name: "Down Mixolydian".into(), intervals: vec![6, 7, 4, 7, 6, 4, 7] },
                Scale { temperament_name: "41-TET".into(), name: "Up Minor".into(), intervals: vec![7, 4, 6, 7, 4, 7, 6] },
                Scale { temperament_name: "41-TET".into(), name: "Up Phrygian".into(), intervals: vec![4, 7, 6, 7, 4, 6, 7] },
                Scale { temperament_name: "41-TET".into(), name: "Up Dorian".into(), intervals: vec![7, 4, 6, 7, 7, 4, 6] },
                Scale { temperament_name: "41-TET".into(), name: "Up Locrian".into(), intervals: vec![4, 6, 7, 3, 8, 6, 7] },
                Scale { temperament_name: "41-TET".into(), name: "Up Lydian".into(), intervals: vec![7, 8, 7, 2, 7, 8, 2] },
                Scale { temperament_name: "41-TET".into(), name: "Up Major".into(), intervals: vec![7, 8, 2, 7, 8, 7, 2] },
                Scale { temperament_name: "41-TET".into(), name: "Up Mixolydian".into(), intervals: vec![8, 7, 2, 7, 8, 2, 7] },
                Scale { temperament_name: "41-TET".into(), name: "Down Minor".into(), intervals: vec![7, 2, 8, 7, 2, 7, 8] },
                Scale { temperament_name: "41-TET".into(), name: "Down Phrygian".into(), intervals: vec![2, 7, 8, 7, 2, 8, 7] },
                Scale { temperament_name: "41-TET".into(), name: "Down Dorian".into(), intervals: vec![7, 2, 8, 7, 7, 2, 8] },
                Scale { temperament_name: "41-TET".into(), name: "Down Locrian".into(), intervals: vec![2, 8, 7, 3, 6, 8, 7] },
            ],
            tunings: vec![Tuning {
                temperament_name: "41-TET".into(),
                name: "Standard Tuning".into(),
                instrument: "Kite Guitar".into(),
                string_tunings: vec![0, 13, 26, 39, 52, 65],
                skip_frets: 1,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
        TemperamentBundle {
            temperament: Temperament {
                name: "Bohlen Pierce".into(),
                divisions: 13,
                period: (3, 1),
            },
            notation_systems: vec![NotationSystem {
                temperament_name: "Bohlen Pierce".into(),
                name: "Standard".into(),
                naturals: vec![
                    Natural { name: "A".into(), degree: 0 },
                    Natural { name: "B".into(), degree: 2 },
                    Natural { name: "C".into(), degree: 3 },
                    Natural { name: "D".into(), degree: 5 },
                    Natural { name: "E".into(), degree: 6 },
                    Natural { name: "F".into(), degree: 7 },
                    Natural { name: "G".into(), degree: 9 },
                    Natural { name: "H".into(), degree: 10 },
                    Natural { name: "J".into(), degree: 12 },
                ],
                accidentals: vec![
                    Accidental { name: "\u{E262}".into(), offset: 1, position: AccidentalPosition::Postfix },
                ],
            }],
            chords: vec![],
            scales: vec![
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Lambda".into(), intervals: vec![2, 1, 1, 2, 1, 2, 1, 2, 1] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Moll 1".into(), intervals: vec![1, 2, 1, 2, 1, 2, 1, 2, 1] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Harmonic".into(), intervals: vec![1, 2, 1, 2, 1, 2, 1, 1, 2] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Dur I".into(), intervals: vec![1, 2, 1, 2, 1, 1, 2, 1, 2] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Moll 2".into(), intervals: vec![2, 1, 2, 1, 1, 2, 1, 2, 1] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Dur II".into(), intervals: vec![2, 1, 1, 2, 1, 2, 1, 1, 2] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Gamma".into(), intervals: vec![1, 2, 1, 2, 1, 1, 2, 2, 1] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Walker A".into(), intervals: vec![1, 1, 2, 1, 2, 1, 2, 1, 2] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Walker B".into(), intervals: vec![1, 2, 1, 1, 2, 1, 2, 1, 2] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Walker I".into(), intervals: vec![2, 1, 2, 1, 2, 1, 2, 1, 1] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Walker II".into(), intervals: vec![2, 1, 2, 1, 2, 1, 1, 2, 1] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Sirius[6]".into(), intervals: vec![2, 2, 2, 2, 2, 3] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Sirius[7]".into(), intervals: vec![2, 2, 2, 2, 2, 2, 1] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Canopus[7]".into(), intervals: vec![3, 1, 3, 1, 3, 1, 1] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Arcturus[5]".into(), intervals: vec![5, 1, 5, 1, 1] },
                Scale { temperament_name: "Bohlen Pierce".into(), name: "Arcturus[7]".into(), intervals: vec![4, 1, 1, 4, 1, 1, 1] },
            ],
            tunings: vec![Tuning {
                temperament_name: "Bohlen Pierce".into(),
                name: "Bohlen's Tuning".into(),
                instrument: "Six String Guitar".into(),
                string_tunings: vec![0, 3, 6, 9, 13, 16],
                skip_frets: 0,
                    fret_markers: vec![],
                root_octave: 2,
            }],
        },
    ]
}

pub fn default_temperaments() -> Vec<Temperament> {
    default_bundles().into_iter().map(|b| b.temperament).collect()
}

pub fn default_notation_systems() -> Vec<NotationSystem> {
    default_bundles().into_iter().flat_map(|b| b.notation_systems).collect()
}

pub fn default_tunings() -> Vec<Tuning> {
    default_bundles().into_iter().flat_map(|b| b.tunings).collect()
}

pub fn default_scales() -> Vec<Scale> {
    default_bundles().into_iter().flat_map(|b| b.scales).collect()
}

pub fn default_chords() -> Vec<Chord> {
    default_bundles().into_iter().flat_map(|b| b.chords).collect()
}
