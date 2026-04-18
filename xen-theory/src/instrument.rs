use crate::temperament::Temperament;
use crate::tuning::{FretMarker, Tuning};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Instrument {
    pub name: String,
    pub instrument_type: String,
    pub temperament_name: String,
    pub num_strings: u32,
    pub num_frets: u32,
    #[cfg_attr(feature = "serde", serde(default))]
    pub fret_markers: Vec<(u32, FretMarker)>,
    /// Per-instrument handedness override. `None` = follow the global preference.
    #[cfg_attr(feature = "serde", serde(default))]
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
