use crate::temperament::Temperament;

#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum FretMarker {
    Single,
    Double,
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

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Tuning {
    pub temperament: Temperament,
    pub name: String,
    pub instrument: String,
    /// Pitch of each string in EDO steps from root
    pub string_tunings: Vec<i32>,
    pub skip_frets: u32,
    /// Neck position markers (inlays). Each entry is (fret_number, marker_type).
    #[cfg_attr(feature = "serde", serde(default))]
    pub fret_markers: Vec<(u32, FretMarker)>,
    /// Octave of the lowest sounding string, matching standard pitch notation
    /// (e.g. E2 → 2, G3 → 3). Used to anchor playback and mic pitch matching
    /// in the correct register. Default: 2 (guitar range).
    #[cfg_attr(feature = "serde", serde(default = "default_root_octave"))]
    pub root_octave: i32,
}

fn default_root_octave() -> i32 {
    2
}

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
