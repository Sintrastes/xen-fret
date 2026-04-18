//! Temperament theory: converting EDO steps to frequencies.

use crate::scale::Scale;

/// Convert an EDO step number to a frequency in Hz.
///
/// `root_hz` is the frequency assigned to step 0.
/// `period` is the period ratio `(numerator, denominator)` — typically `(2, 1)` for an octave.
pub fn edo_step_to_hz(step: i32, divisions: u32, period: (u32, u32), root_hz: f64) -> f64 {
    let period_ratio = period.0 as f64 / period.1 as f64;
    root_hz * period_ratio.powf(step as f64 / divisions as f64)
}

/// Build the ascending Hz sequence for one period of `scale`, rooted at `key`.
///
/// `key` is the number of EDO steps above step 0 (A4 = 440 Hz) where the root sits.
/// Returns `intervals.len() + 1` values: root through the octave repetition of the root.
pub fn scale_to_hz_sequence(
    scale: &Scale,
    key: i32,
    divisions: u32,
    period: (u32, u32),
    root_hz: f64,
) -> Vec<f64> {
    if divisions == 0 || scale.intervals.is_empty() {
        return vec![];
    }
    // Frequency of the scale root.
    let root = edo_step_to_hz(key, divisions, period, root_hz);

    // Cumulative steps within the scale: [0, i0, i0+i1, …, sum(all)].
    let mut steps = Vec::with_capacity(scale.intervals.len() + 1);
    let mut acc = 0i32;
    steps.push(0i32);
    for &interval in &scale.intervals {
        acc += interval;
        steps.push(acc);
    }

    steps
        .iter()
        .map(|&s| edo_step_to_hz(s, divisions, period, root))
        .collect()
}

/// Return the absolute EDO steps for one period of `scale` rooted at `key`.
///
/// E.g. for a major scale (intervals [2,2,1,2,2,2,1]) with key=7 (E in 12-TET):
/// returns [7, 9, 11, 12, 14, 16, 18, 19].
pub fn scale_absolute_steps(scale: &Scale, key: i32) -> Vec<i32> {
    let mut steps = Vec::with_capacity(scale.intervals.len() + 1);
    let mut acc = 0i32;
    steps.push(key);
    for &interval in &scale.intervals {
        acc += interval;
        steps.push(key + acc);
    }
    steps
}

/// Convert a frequency in Hz to the nearest MIDI note number and its deviation in cents.
///
/// Returns `(midi_note, cents)` where `cents` is in `(-50, 50]`.
/// Useful for MIDI export and debug display.
pub fn hz_to_midi_and_bend(hz: f64) -> (u8, f64) {
    if hz <= 0.0 {
        return (69, 0.0);
    }
    let midi_float = 69.0 + 12.0 * (hz / 440.0).log2();
    let midi_note = midi_float.round().clamp(0.0, 127.0) as u8;
    let note_hz = 440.0 * 2.0_f64.powf((midi_note as f64 - 69.0) / 12.0);
    let cents = 1200.0 * (hz / note_hz).log2();
    (midi_note, cents)
}
