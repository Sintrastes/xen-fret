//! Pure fretboard math: fret position calculation and note mapping.
//!
//! No rendering dependencies — all functions return plain Rust types.

use crate::scale::Scale;
use crate::tuning::Tuning;

// ── Note ──────────────────────────────────────────────────────────────────────

/// A note occurrence on a specific fret of a specific string.
#[derive(Debug, Clone)]
pub struct Note {
    pub scale_degree: usize,
    /// Fret number (0 = open string).
    pub pitch: i32,
    /// Absolute EDO step of this note (= raw_pitch + key).
    /// Used for octave-aware "is this note currently playing?" checks.
    pub absolute_step: i32,
}

// ── fret_pos ──────────────────────────────────────────────────────────────────

/// Normalized position of fret `k` along a neck of `n` frets.
///
/// The tuning has `edo` equal divisions of `period` (e.g. `period=2.0` for an
/// octave, `period=3.0` for Bohlen-Pierce tritave). Returns `0.0` at the nut
/// and `1.0` at the last fret.
pub fn fret_pos(k: f64, n: f64, edo: f64, period: f64) -> f64 {
    if n <= 0.0 || edo <= 0.0 || period <= 1.0 {
        return k / n.max(1.0);
    }
    let denom = 1.0 - period.powf(-n / edo);
    if denom.abs() < 1e-12 {
        return k / n;
    }
    (1.0 - period.powf(-k / edo)) / denom
}

// ── repeating_notes ───────────────────────────────────────────────────────────

/// Infinite iterator yielding `(scale_degree_index, accumulated_edo_steps)`
/// for every repetition of the scale across all periods.
pub fn repeating_notes(scale: &Scale) -> impl Iterator<Item = (usize, i32)> + '_ {
    let period_size: i32 = scale.intervals.iter().sum();
    let len = scale.intervals.len();
    let mut current = -period_size;
    let mut j = 0usize;
    std::iter::from_fn(move || {
        if len == 0 {
            return None;
        }
        let degree = j % len;
        let item = (degree, current);
        current += scale.intervals[j % len];
        j += 1;
        Some(item)
    })
}

// ── get_notes ─────────────────────────────────────────────────────────────────

/// For each string of `tuning`, return all [`Note`]s that fall on the
/// fretboard at integer fret positions given `skip_frets` and a max of 36 frets.
pub fn get_notes(scale: &Scale, tuning: &Tuning, key: i32, skip_frets: u32) -> Vec<Vec<Note>> {
    let n = (skip_frets + 1) as i32;
    let period_size: i32 = scale.intervals.iter().sum();
    if period_size <= 0 || scale.intervals.is_empty() {
        return tuning.string_tunings.iter().map(|_| vec![]).collect();
    }
    const MAX_FRET: i32 = 36;
    tuning
        .string_tunings
        .iter()
        .map(|&string_pitch| {
            let mut notes = Vec::new();
            for (degree, raw_pitch) in repeating_notes(scale) {
                let numerator = raw_pitch + key - string_pitch;
                if numerator % n == 0 {
                    let fret = numerator / n;
                    if fret >= 0 {
                        if fret > MAX_FRET {
                            break;
                        }
                        notes.push(Note {
                            scale_degree: degree,
                            pitch: fret,
                            absolute_step: raw_pitch + key,
                        });
                    }
                }
                if raw_pitch - string_pitch + key > (MAX_FRET + 2) * n + period_size {
                    break;
                }
            }
            notes
        })
        .collect()
}
