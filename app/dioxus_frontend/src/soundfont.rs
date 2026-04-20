//! Offline audio rendering via a bundled General MIDI SoundFont (SF2).
//!
//! Uses `rustysynth` to synthesize `NoteTimeline` events into PCM audio,
//! then encodes as WAV for playback through the Web Audio API.
//!
//! Place a GM-compatible SF2 file at `assets/soundfont.sf2`.  If the file
//! is absent the module gracefully reports "not available" and the app falls
//! back to the sample-fetching JS engine.

use std::io::Cursor;
use std::sync::{Arc, OnceLock};

use rustysynth::{SoundFont, Synthesizer, SynthesizerSettings};

use xen_sequencer::sequencer::NoteTimeline;

// ── Constants ───────────────────────────────────────────────────────────────

const SAMPLE_RATE: i32 = 44100;

/// GM program numbers for common instruments.
pub const GM_ACOUSTIC_GUITAR_NYLON: u8 = 24;
pub const GM_ACOUSTIC_GUITAR_STEEL: u8 = 25;
pub const GM_ACOUSTIC_BASS: u8 = 32;
pub const GM_ELECTRIC_BASS_FINGER: u8 = 33;
pub const GM_ACOUSTIC_GRAND_PIANO: u8 = 0;

// ── Global cached soundfont ─────────────────────────────────────────────────

static SOUNDFONT: OnceLock<Option<Arc<SoundFont>>> = OnceLock::new();

/// Get a reference to the loaded soundfont, loading it on first call.
/// Returns `None` if no valid SF2 was bundled.
pub fn get_soundfont() -> Option<&'static Arc<SoundFont>> {
    SOUNDFONT.get_or_init(load_bundled).as_ref()
}

/// Check if a soundfont is available without loading it.
pub fn is_available() -> bool {
    get_soundfont().is_some()
}

// ── SF2 loading ─────────────────────────────────────────────────────────────

/// Load the SF2 that was embedded at compile time via `include_bytes!`.
fn load_bundled() -> Option<Arc<SoundFont>> {
    #[cfg(has_soundfont)]
    {
        let bytes: &[u8] = include_bytes!("../assets/soundfont.sf2");
        return load_from_bytes(bytes);
    }
    #[cfg(not(has_soundfont))]
    None
}

/// Parse an SF2 from raw bytes.  Returns `None` if too small or invalid.
pub fn load_from_bytes(bytes: &[u8]) -> Option<Arc<SoundFont>> {
    if bytes.len() < 12 {
        return None;
    }
    if &bytes[0..4] != b"RIFF" {
        return None;
    }
    let mut cursor = Cursor::new(bytes);
    SoundFont::new(&mut cursor).ok().map(Arc::new)
}

// ── Timeline → PCM rendering ────────────────────────────────────────────────

/// An event with an associated GM program (instrument).
#[derive(Debug, Clone)]
pub struct InstrumentEvent {
    pub start_s: f64,
    pub duration_s: f64,
    pub freq_hz: f64,
    pub velocity: f64,
    /// GM program number (0–127).  Channel 9 is always percussion.
    pub program: u8,
    /// MIDI channel (0–15).  Use 9 for drums.
    pub channel: u8,
}

/// A complete timeline with instrument assignments, ready for SF2 rendering.
#[derive(Debug, Clone)]
pub struct InstrumentTimeline {
    pub events: Vec<InstrumentEvent>,
    pub total_duration_s: f64,
}

/// Convert Hz to the nearest MIDI key (0–127) and pitch-bend fraction.
fn hz_to_midi_key(hz: f64) -> (i32, f64) {
    let exact = 69.0 + 12.0 * (hz / 440.0).log2();
    let key = exact.round() as i32;
    let bend = exact - key as f64;
    (key.clamp(0, 127), bend)
}

/// Melodic channels available for allocation (skip 9 = drums).
const MELODIC_CHS: [i32; 15] = [0,1,2,3,4,5,6,7,8,10,11,12,13,14,15];

/// Allocate melodic channels so each concurrent note gets its own pitch bend.
struct ChannelAlloc {
    /// For each of the 15 melodic channel slots, the sample at which it becomes free.
    free_at: [usize; 15],
}

impl ChannelAlloc {
    fn new() -> Self { Self { free_at: [0; 15] } }

    /// Reserve a channel that is free at `start`, occupied until `end`.
    fn alloc(&mut self, start: usize, end: usize) -> i32 {
        // Prefer the channel that has been free the longest.
        let best = self.free_at.iter().enumerate()
            .filter(|(_, &f)| f <= start)
            .min_by_key(|(_, &f)| f)
            .map(|(i, _)| i)
            // If all are busy, steal the one that frees soonest.
            .unwrap_or_else(|| {
                self.free_at.iter().enumerate()
                    .min_by_key(|(_, &f)| f)
                    .map(|(i, _)| i)
                    .unwrap_or(0)
            });
        self.free_at[best] = end;
        MELODIC_CHS[best]
    }
}

/// Convert a pitch-bend in semitones (±2 range) to MIDI pitch bend LSB/MSB.
fn bend_to_midi(semitones: f64) -> (i32, i32) {
    let clamped = semitones.clamp(-2.0, 2.0);
    let value = ((clamped / 2.0) * 8192.0 + 8192.0).round() as i32;
    let value = value.clamp(0, 16383);
    (value & 0x7F, (value >> 7) & 0x7F)
}

/// Render an `InstrumentTimeline` to WAV bytes.
///
/// Melodic notes are assigned to individual MIDI channels so that each
/// concurrent note can have its own pitch bend — essential for microtonal
/// temperaments (22-TET, 31-TET, etc.) where pitches fall between 12-TET keys.
pub fn render_timeline(sf: &Arc<SoundFont>, timeline: &InstrumentTimeline) -> Option<Vec<u8>> {
    let settings = SynthesizerSettings::new(SAMPLE_RATE);
    let mut synth = Synthesizer::new(sf, &settings).ok()?;

    let total_samples = ((timeline.total_duration_s + 0.5) * SAMPLE_RATE as f64).ceil() as usize;
    let mut left = vec![0f32; total_samples];
    let mut right = vec![0f32; total_samples];

    let mut sorted: Vec<_> = timeline.events.iter().collect();
    sorted.sort_by(|a, b| a.start_s.partial_cmp(&b.start_s).unwrap());

    #[derive(Debug)]
    enum Action {
        ProgramChange { channel: i32, program: i32 },
        PitchBend { channel: i32, lsb: i32, msb: i32 },
        NoteOn { channel: i32, key: i32, velocity: i32 },
        NoteOff { channel: i32, key: i32 },
    }

    let mut schedule: Vec<(usize, Action)> = Vec::new();
    let mut channel_program = [255u8; 16];
    let mut alloc = ChannelAlloc::new();

    for ev in &sorted {
        let start_sample = (ev.start_s * SAMPLE_RATE as f64).round() as usize;
        let end_sample = ((ev.start_s + ev.duration_s) * SAMPLE_RATE as f64).round() as usize;

        if ev.channel == 9 {
            // Drums: channel 9, note maps directly to percussion instrument.
            let (key, _) = hz_to_midi_key(ev.freq_hz);
            let vel = (ev.velocity * 127.0).round() as i32;
            schedule.push((start_sample, Action::NoteOn { channel: 9, key, velocity: vel }));
            schedule.push((end_sample.min(total_samples), Action::NoteOff { channel: 9, key }));
        } else {
            // Melodic: allocate a dedicated channel for pitch-bend isolation.
            let ch = alloc.alloc(start_sample, end_sample);

            if channel_program[ch as usize] != ev.program {
                channel_program[ch as usize] = ev.program;
                schedule.push((start_sample, Action::ProgramChange {
                    channel: ch,
                    program: ev.program as i32,
                }));
            }

            let (key, bend) = hz_to_midi_key(ev.freq_hz);
            let vel = (ev.velocity * 127.0).round() as i32;

            // Always set pitch bend so it doesn't carry over from a previous note.
            let (lsb, msb) = bend_to_midi(bend);
            schedule.push((start_sample, Action::PitchBend { channel: ch, lsb, msb }));
            schedule.push((start_sample, Action::NoteOn { channel: ch, key, velocity: vel }));
            schedule.push((end_sample.min(total_samples), Action::NoteOff { channel: ch, key }));
        }
    }

    // Sort: program changes first, then pitch bends, then note-ons, then note-offs.
    schedule.sort_by(|a, b| {
        a.0.cmp(&b.0).then_with(|| {
            let order = |action: &Action| match action {
                Action::ProgramChange { .. } => 0,
                Action::PitchBend { .. } => 1,
                Action::NoteOn { .. } => 2,
                Action::NoteOff { .. } => 3,
            };
            order(&a.1).cmp(&order(&b.1))
        })
    });

    let mut cursor = 0usize;
    let mut sched_idx = 0;

    while cursor < total_samples {
        let next_event_sample = if sched_idx < schedule.len() {
            schedule[sched_idx].0
        } else {
            total_samples
        };

        let render_to = next_event_sample.min(total_samples);
        if render_to > cursor {
            synth.render(&mut left[cursor..render_to], &mut right[cursor..render_to]);
            cursor = render_to;
        }

        while sched_idx < schedule.len() && schedule[sched_idx].0 <= cursor {
            match &schedule[sched_idx].1 {
                Action::ProgramChange { channel, program } => {
                    synth.process_midi_message(*channel, 0xC0, *program, 0);
                }
                Action::PitchBend { channel, lsb, msb } => {
                    synth.process_midi_message(*channel, 0xE0, *lsb, *msb);
                }
                Action::NoteOn { channel, key, velocity } => {
                    synth.note_on(*channel, *key, *velocity);
                }
                Action::NoteOff { channel, key } => {
                    synth.note_off(*channel, *key);
                }
            }
            sched_idx += 1;
        }
    }

    Some(encode_wav(&left, &right, SAMPLE_RATE))
}

/// Render a basic NoteTimeline (no instrument info) using a default program.
pub fn render_basic_timeline(
    sf: &Arc<SoundFont>,
    timeline: &NoteTimeline,
    program: u8,
) -> Option<Vec<u8>> {
    let inst_timeline = InstrumentTimeline {
        total_duration_s: timeline.total_duration_s,
        events: timeline.events.iter().map(|e| InstrumentEvent {
            start_s: e.start_s,
            duration_s: e.duration_s,
            freq_hz: e.freq_hz,
            velocity: e.velocity,
            program,
            channel: 0,
        }).collect(),
    };
    render_timeline(sf, &inst_timeline)
}

// ── WAV encoder ─────────────────────────────────────────────────────────────

fn encode_wav(left: &[f32], right: &[f32], sample_rate: i32) -> Vec<u8> {
    let num_samples = left.len();
    let num_channels: u16 = 2;
    let bits_per_sample: u16 = 16;
    let byte_rate = sample_rate as u32 * num_channels as u32 * (bits_per_sample / 8) as u32;
    let block_align = num_channels * (bits_per_sample / 8);
    let data_size = (num_samples * num_channels as usize * (bits_per_sample / 8) as usize) as u32;
    let file_size = 36 + data_size;

    let mut buf = Vec::with_capacity(file_size as usize + 8);

    buf.extend_from_slice(b"RIFF");
    buf.extend_from_slice(&file_size.to_le_bytes());
    buf.extend_from_slice(b"WAVE");

    buf.extend_from_slice(b"fmt ");
    buf.extend_from_slice(&16u32.to_le_bytes());
    buf.extend_from_slice(&1u16.to_le_bytes());
    buf.extend_from_slice(&num_channels.to_le_bytes());
    buf.extend_from_slice(&(sample_rate as u32).to_le_bytes());
    buf.extend_from_slice(&byte_rate.to_le_bytes());
    buf.extend_from_slice(&block_align.to_le_bytes());
    buf.extend_from_slice(&bits_per_sample.to_le_bytes());

    buf.extend_from_slice(b"data");
    buf.extend_from_slice(&data_size.to_le_bytes());

    for i in 0..num_samples {
        let l = (left[i].clamp(-1.0, 1.0) * 32767.0) as i16;
        let r = (right[i].clamp(-1.0, 1.0) * 32767.0) as i16;
        buf.extend_from_slice(&l.to_le_bytes());
        buf.extend_from_slice(&r.to_le_bytes());
    }

    buf
}
