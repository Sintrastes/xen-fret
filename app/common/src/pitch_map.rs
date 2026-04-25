//! Maps detected pitches to absolute EDO steps on the current fretboard.
//!
//! Extracted from `dioxus_frontend/src/pages/home.rs` so all frontends share the
//! same mapping logic.

use crate::state::AppState;
use xen_dsp::detector::DetectedPitch;

/// Convert a slice of detected pitches to absolute EDO steps visible on the
/// fretboard, with a lowest-string offset correction so octave-shifted notes
/// land on the right fret positions.
///
/// Filters by confidence ≥ 0.1 and 50-cent scale-degree tolerance.
pub fn detected_to_absolute_steps(
    state: &AppState,
    pitches: &[DetectedPitch],
) -> Vec<i32> {
    let Some(temp) = state.current_temperament() else { return vec![] };
    let Some(scale) = state.current_scale() else { return vec![] };
    let Some(tuning) = state.current_tuning() else { return vec![] };
    let concert_hz = state.preferences.concert_hz;
    let key = state.diagram_settings.key as i32;
    let period_tuple = (*temp.period.numer(), *temp.period.denom());

    let step0_hz = tuning.step0_hz(
        concert_hz,
        state.preferences.concert_octave,
        temp.divisions,
        period_tuple,
    );

    let period_ratio = temp.period_f64();
    let lowest_step = tuning.string_tunings.iter().min().copied().unwrap_or(0);
    let lowest_hz = step0_hz * period_ratio.powf(lowest_step as f64 / temp.divisions as f64);
    let lowest_abs = (temp.divisions as f64 * (lowest_hz / concert_hz).ln() / period_ratio.ln())
        .round() as i32;
    let offset = lowest_step - lowest_abs;

    pitches
        .iter()
        .filter(|p| p.confidence > 0.1)
        .filter_map(|p| {
            xen_dsp::pitch_tracking::hz_to_absolute_step(
                p.hz,
                temp.divisions,
                period_tuple,
                concert_hz,
                key,
                &scale.intervals,
                50.0,
            )
            .map(|s| s + offset)
        })
        .collect()
}
