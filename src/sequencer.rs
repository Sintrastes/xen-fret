//! Music sequencer: builds timed chord sequences from scale degrees and chord types.
//!
//! The input representation uses scale degrees and chord names (or inline intervals),
//! which are resolved against a temperament to produce a `NoteTimeline` — an absolute-
//! time list of `NoteEvent`s suitable for playback via Web Audio or export to MIDI.

use crate::models::Chord;
use crate::theory;

// ── Strum parameters ────────────────────────────────────────────────────────

/// Default inter-string delay for strummed chords (15 ms).
const STRUM_DELAY_S: f64 = 0.015;

/// Maximum fraction of the chord duration that the strum spread may occupy.
const MAX_STRUM_FRACTION: f64 = 0.3;

// ── Input types (high-level, temperament-aware) ─────────────────────────────

/// How to specify a chord's interval structure.
#[derive(Debug, Clone, PartialEq)]
pub enum ChordSpec {
    /// Look up by name from the temperament's chord list (e.g. "Major").
    Named(String),
    /// Provide EDO-step intervals directly (successive, e.g. `[10, 8, 13]`).
    Intervals(Vec<i32>),
}

/// Direction of a guitar strum.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StrumDirection {
    /// Low string to high string.
    Down,
    /// High string to low string.
    Up,
    /// All strings struck at the same instant.
    Simultaneous,
}

/// One chord hit within a bar.
#[derive(Debug, Clone, PartialEq)]
pub struct ChordEntry {
    /// 1-indexed scale degree (1 = root, 4 = fourth, 5 = fifth, etc.).
    pub scale_degree: u32,
    /// Which chord type to play on that degree.
    pub chord: ChordSpec,
    /// Strum articulation.
    pub strum: StrumDirection,
    /// Duration of this chord in beats.
    pub duration_beats: f64,
    /// Shift the bass note down (negative) or up (positive) by this many octaves.
    /// Useful for inversions / slash chords (e.g. A/C# needs degree 7 shifted -1).
    pub octave_offset: i32,
}

/// A bar of music.
#[derive(Debug, Clone, PartialEq)]
pub struct Bar {
    pub events: Vec<ChordEntry>,
}

/// Complete sequence description — temperament-aware input.
#[derive(Debug, Clone, PartialEq)]
pub struct Sequence {
    pub temperament_name: String,
    /// Scale intervals (successive EDO steps, e.g. Ionian: `[2,2,1,2,2,2,1]`).
    pub scale_intervals: Vec<i32>,
    /// Root key as an EDO step offset from step 0 (A4 = 440 Hz).
    pub key: i32,
    /// Octave offset for register placement (e.g. -2 for guitar register).
    pub octave: i32,
    /// Number of equal divisions per period (e.g. 12, 22, 31).
    pub divisions: u32,
    /// Period ratio, typically `(2, 1)` for an octave.
    pub period: (u32, u32),
    /// Tempo in beats per minute.
    pub bpm: f64,
    /// Beats per bar (e.g. 4 for 4/4 time).
    pub beats_per_bar: u32,
    /// The bars to play.
    pub bars: Vec<Bar>,
}

// ── IR types (resolved, ready for playback or MIDI export) ──────────────────

/// A single timed note event.
#[derive(Debug, Clone, PartialEq)]
pub struct NoteEvent {
    /// Absolute start time in seconds from the beginning of the sequence.
    pub start_s: f64,
    /// Duration in seconds.
    pub duration_s: f64,
    /// Frequency in Hz (supports microtonal — not quantized to MIDI).
    pub freq_hz: f64,
    /// Velocity / loudness, 0.0–1.0.
    pub velocity: f64,
}

/// Fully resolved timeline of note events.
#[derive(Debug, Clone, PartialEq)]
pub struct NoteTimeline {
    pub events: Vec<NoteEvent>,
    /// Total duration of the sequence in seconds.
    pub total_duration_s: f64,
}

// ── Fallback chord computation ──────────────────────────────────────────────

/// Compute standard chord intervals from 5-limit just ratios for any EDO.
/// Returns successive intervals (e.g. Major → [M3, m3, P4]).
fn fallback_chord(name: &str, divisions: u32) -> Vec<i32> {
    let n = divisions as f64;
    let m3 = (n * (6.0_f64 / 5.0).log2()).round() as i32;
    let maj3 = (n * (5.0_f64 / 4.0).log2()).round() as i32;
    let p5 = (n * (3.0_f64 / 2.0).log2()).round() as i32;
    let p4 = divisions as i32 - p5;
    let min7 = (n * (9.0_f64 / 5.0).log2()).round() as i32;
    let maj7 = (n * (15.0_f64 / 8.0).log2()).round() as i32;

    match name {
        "Major" => vec![maj3, p5 - maj3, p4],
        "Minor" => vec![m3, p5 - m3, p4],
        "Major 7th" | "Major7" | "Maj7" => vec![maj3, p5 - maj3, maj7 - p5, divisions as i32 - maj7],
        "Minor 7th" | "Minor7" | "Min7" => vec![m3, p5 - m3, min7 - p5, divisions as i32 - min7],
        "Dominant 7th" | "Dom7" | "7" => vec![maj3, p5 - maj3, min7 - p5, divisions as i32 - min7],
        _ => vec![], // truly unknown chord
    }
}

// ── Compiler: Sequence → NoteTimeline ───────────────────────────────────────

/// Compile a high-level `Sequence` into a concrete `NoteTimeline`.
///
/// `concert_hz` is the frequency of step 0 in octave 0 (e.g. 440 Hz for A4).
/// `chords` is the full chord list from `AppState` (filtered by temperament internally).
pub fn compile(seq: &Sequence, concert_hz: f64, chords: &[Chord]) -> NoteTimeline {
    let beat_s = 60.0 / seq.bpm;
    let period_ratio = seq.period.0 as f64 / seq.period.1 as f64;
    let root_hz = concert_hz * period_ratio.powi(seq.octave);

    // Build absolute scale degree steps: [key, key+i0, key+i0+i1, ...]
    let scale_steps = {
        let mut steps = Vec::with_capacity(seq.scale_intervals.len() + 1);
        let mut acc = seq.key;
        steps.push(acc);
        for &interval in &seq.scale_intervals {
            acc += interval;
            steps.push(acc);
        }
        steps
    };

    let period_steps = seq.divisions as i32; // one full period in EDO steps

    let mut events = Vec::new();
    let mut clock = 0.0_f64;

    // Minimum sustain: guitar strings ring well beyond the strum gap.
    // We let each note sustain until the *end of its bar*, not just until
    // the next strum.  This prevents the staccato "chop" effect.
    for bar in &seq.bars {
        let bar_dur_s: f64 = bar.events.iter().map(|e| e.duration_beats).sum::<f64>() * beat_s;
        let bar_start = clock;

        for entry in &bar.events {
            let chord_dur_s = entry.duration_beats * beat_s;
            // Each note rings from its onset until the end of the bar.
            let ring_dur_s = (bar_start + bar_dur_s) - clock;

            // Resolve scale degree to EDO step (1-indexed, wrapping into higher octaves).
            let degree_idx = (entry.scale_degree as usize).saturating_sub(1);
            let n_degrees = scale_steps.len().saturating_sub(1).max(1);
            let octave_wrap = degree_idx / n_degrees;
            let wrapped_idx = degree_idx % n_degrees;
            let root_step = scale_steps.get(wrapped_idx).copied().unwrap_or(seq.key)
                + (octave_wrap as i32) * period_steps
                + entry.octave_offset * period_steps;

            // Resolve chord intervals.
            let raw_intervals: Vec<i32> = match &entry.chord {
                ChordSpec::Named(name) => {
                    chords
                        .iter()
                        .find(|c| c.temperament_name == seq.temperament_name && c.name == *name)
                        .map(|c| c.intervals.clone())
                        // Fallback: compute standard triads from 5-limit ratios for any EDO.
                        .unwrap_or_else(|| fallback_chord(name, seq.divisions))
                }
                ChordSpec::Intervals(v) => v.clone(),
            };

            // Some temperaments (e.g. 22-TET) include a leading 0 for the root in
            // their chord intervals; skip it since we already include the root.
            let intervals = if raw_intervals.first() == Some(&0) {
                &raw_intervals[1..]
            } else {
                &raw_intervals[..]
            };

            // Build absolute steps for each chord voice.
            let mut voice_steps = Vec::with_capacity(intervals.len() + 1);
            voice_steps.push(root_step);
            let mut acc = root_step;
            for &iv in intervals {
                acc += iv;
                voice_steps.push(acc);
            }

            // Convert to Hz.
            let freqs: Vec<f64> = voice_steps
                .iter()
                .map(|&s| theory::edo_step_to_hz(s, seq.divisions, seq.period, root_hz))
                .collect();

            // Compute strum delay, clamped for fast tempos.
            let n_voices = freqs.len();
            let strum_delay = if n_voices <= 1 || entry.strum == StrumDirection::Simultaneous {
                0.0
            } else {
                let max_delay = chord_dur_s * MAX_STRUM_FRACTION / (n_voices - 1) as f64;
                STRUM_DELAY_S.min(max_delay)
            };

            // Emit note events with strum offsets and velocity tapering.
            for (i, &hz) in freqs.iter().enumerate() {
                let offset = match entry.strum {
                    StrumDirection::Down => i as f64 * strum_delay,
                    StrumDirection::Up => (n_voices - 1 - i) as f64 * strum_delay,
                    StrumDirection::Simultaneous => 0.0,
                };

                // Velocity taper: first-struck string loudest.
                let strike_order = match entry.strum {
                    StrumDirection::Down => i,
                    StrumDirection::Up => n_voices - 1 - i,
                    StrumDirection::Simultaneous => 0,
                };
                let velocity = if n_voices <= 1 {
                    1.0
                } else {
                    1.0 - 0.15 * (strike_order as f64 / (n_voices - 1) as f64)
                };

                events.push(NoteEvent {
                    start_s: clock + offset,
                    duration_s: (ring_dur_s - offset).max(0.05),
                    freq_hz: hz,
                    velocity,
                });
            }

            clock += chord_dur_s;
        }
    }

    NoteTimeline {
        total_duration_s: clock,
        events,
    }
}

// ── Demo: "Komm Süsser Tod" ────────────────────────────────────────────────

/// Build a demo sequence of the "Komm Süsser Tod" opening chords for the given
/// temperament.  Returns `(Sequence, auxiliary_chords)` where auxiliary_chords
/// contains any inline chord definitions needed for `compile`.
///
/// Supported temperaments: "12-TET", "22-TET", "31-TET".
/// Falls back to 12-TET for anything else.
pub fn komm_susser_tod_demo(temperament: &str) -> (Sequence, Vec<Chord>) {
    // The opening verse has a descending bass line over the D major scale:
    //
    //   D → A/C# → Bm → Bm/A → G → D/F# → Em7 → A
    //
    // Each chord lasts 2 beats in 4/4 at ~76 BPM (gentle ballad feel).
    // Strum pattern per 2-beat chord: Down(1.0) – Up(0.5) – Down(0.5).
    //
    // Voicings use open-position guitar shapes.  Root scale degrees are placed
    // by the circle-of-fifths scale (important for superpyth), but chord
    // intervals use pure 5-limit ratios for consonance.

    // ── 5-limit interval table per temperament ──────────────────────────
    //
    //  Quality  Ratio   12-TET  22-TET  31-TET
    //  m3       6:5     3       6       8
    //  M3       5:4     4       7       10
    //  P4       4:3     5       9       13
    //  P5       3:2     7       13      18
    //  m6       8:5     8       15      21
    //  M6       5:3     9       16      23
    //
    // For 22-TET superpyth the scale generator is the P5 (13 steps), so root
    // degrees follow the circle of fifths [4,4,1,4,4,4,1].  But chord tones
    // use the pure M3=7, m3=6 (not the Pythagorean M3=8 from 4 fifths).

    /// Interval qualities used to define chord voicings.
    #[derive(Clone, Copy)]
    enum Iv { M3, m3, P4, P5, m6, M6 }

    /// Resolve an interval quality to EDO steps for the given temperament.
    fn resolve(iv: Iv, divisions: u32) -> i32 {
        match (iv, divisions) {
            // 12-TET
            (Iv::m3, 12) => 3,  (Iv::M3, 12) => 4,  (Iv::P4, 12) => 5,
            (Iv::P5, 12) => 7,  (Iv::m6, 12) => 8,  (Iv::M6, 12) => 9,
            // 22-TET (pure 5-limit thirds, Pythagorean fifths)
            (Iv::m3, 22) => 6,  (Iv::M3, 22) => 7,  (Iv::P4, 22) => 9,
            (Iv::P5, 22) => 13, (Iv::m6, 22) => 15, (Iv::M6, 22) => 16,
            // 31-TET (meantone)
            (Iv::m3, 31) => 8,  (Iv::M3, 31) => 10, (Iv::P4, 31) => 13,
            (Iv::P5, 31) => 18, (Iv::m6, 31) => 21, (Iv::M6, 31) => 23,
            // Fallback: 12-TET values
            (Iv::m3, _) => 3,  (Iv::M3, _) => 4,  (Iv::P4, _) => 5,
            (Iv::P5, _) => 7,  (Iv::m6, _) => 8,  (Iv::M6, _) => 9,
        }
    }

    fn resolve_ivs(ivs: &[Iv], divisions: u32) -> Vec<i32> {
        ivs.iter().map(|&iv| resolve(iv, divisions)).collect()
    }

    // Guitar voicings expressed as interval qualities from bass note:
    //
    //  Chord    Bass   Voicing                          Interval qualities
    //  D        D3     D3  A3  D4  F#4                  P5, P4, M3
    //  A/C#     C#3    C#3 E3  A3  C#4                  m3, P4, M3
    //  Bm       B2     B2  F#3 B3  D4  F#4              P5, P4, m3, M3
    //  Bm/A     A2     A2  F#3 B3  D4  F#4              M6, P4, m3, M3
    //  G        G2     G2  B2  D3  G3  B3  G4           M3, m3, P4, M3, m6
    //  D/F#     F#2    F#2 A2  D3  A3  D4  F#4          m3, P4, P5, P4, M3
    //  Em7      E2     E2  B2  D3  G3  B3  E4           P5, m3, P4, M3, P4
    //  A        A2     A2  E3  A3  C#4 E4               P5, P4, M3, m3

    use Iv::*;
    struct Voice { degree: u32, oct: i32, ivs: &'static [Iv] }

    let chords: &[Voice] = &[
        Voice { degree: 1, oct: 0,  ivs: &[P5, P4, M3] },          // D
        Voice { degree: 7, oct: -1, ivs: &[m3, P4, M3] },          // A/C#
        Voice { degree: 6, oct: -1, ivs: &[P5, P4, m3, M3] },      // Bm
        Voice { degree: 5, oct: -1, ivs: &[M6, P4, m3, M3] },      // Bm/A
        Voice { degree: 4, oct: -1, ivs: &[M3, m3, P4, M3, m6] },  // G
        Voice { degree: 3, oct: -1, ivs: &[m3, P4, P5, P4, M3] },  // D/F#
        Voice { degree: 2, oct: -1, ivs: &[P5, m3, P4, M3, P4] },  // Em7
        Voice { degree: 5, oct: -1, ivs: &[P5, P4, M3, m3] },      // A
    ];

    // Scale & key per temperament.  22-TET uses superpyth (circle-of-fifths
    // generator = 13) for scale degrees; 31-TET uses meantone.
    let (temp_name, scale_intervals, key, divisions): (&str, Vec<i32>, i32, u32) =
        match temperament {
            "22-TET" => ("22-TET", vec![4, 4, 1, 4, 4, 4, 1], 9,  22),
            "31-TET" => ("31-TET", vec![5, 5, 3, 5, 5, 5, 3], 13, 31),
            _        => ("12-TET", vec![2, 2, 1, 2, 2, 2, 1], 5,  12),
        };

    // Build bars: 4 bars × 4/4, two chords per bar (2 beats each).
    // Strum pattern per chord: Down(1.0) – Up(0.5) – Down(0.5).
    let mut bars = Vec::new();
    for pair in chords.chunks(2) {
        let mut bar_events = Vec::new();
        for v in pair {
            let ivs = resolve_ivs(v.ivs, divisions);
            for &(strum, dur) in &[
                (StrumDirection::Down, 1.0),
                (StrumDirection::Up,   0.5),
                (StrumDirection::Down, 0.5),
            ] {
                bar_events.push(ChordEntry {
                    scale_degree: v.degree,
                    chord: ChordSpec::Intervals(ivs.clone()),
                    strum,
                    duration_beats: dur,
                    octave_offset: v.oct,
                });
            }
        }
        bars.push(Bar { events: bar_events });
    }

    (
        Sequence {
            temperament_name: temp_name.into(),
            scale_intervals,
            key,
            octave: -2,
            divisions,
            period: (2, 1),
            bpm: 76.0,
            beats_per_bar: 4,
            bars,
        },
        vec![],
    )
}
