//! Native audio backend using rodio for output and rustysynth for SF2 synthesis.
//!
//! Pass a pre-loaded `Arc<SoundFont>` to `NativeAudioBackend::new` for guitar-quality
//! tone.  If no soundfont is provided (or SF2 parsing fails) the backend falls back to
//! simple sine-wave oscillators — exactly matching the web oscillator fallback.

use std::io::Cursor;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use rodio::{Decoder, OutputStream, OutputStreamHandle, Sink, Source};
use rustysynth::{SoundFont, Synthesizer, SynthesizerSettings};

use crate::backend::AudioBackend;
use crate::sequencer::NoteTimeline;

const SAMPLE_RATE: i32 = 44_100;

// ── Backend struct ────────────────────────────────────────────────────────────

pub struct NativeAudioBackend {
    _stream: OutputStream,
    handle: OutputStreamHandle,
    sf: Option<Arc<SoundFont>>,
    active: Arc<Mutex<Vec<Arc<Sink>>>>,
}

impl NativeAudioBackend {
    /// Create a new backend. `sf` is an optional pre-loaded SoundFont for synthesis;
    /// pass `None` to use sine-wave oscillators.
    /// Returns an error if no audio output device is available.
    pub fn new(sf: Option<Arc<SoundFont>>) -> Result<Self, rodio::StreamError> {
        let (stream, handle) = OutputStream::try_default()?;
        Ok(Self { _stream: stream, handle, sf, active: Default::default() })
    }
}

impl AudioBackend for NativeAudioBackend {
    fn stop_all(&self) {
        for s in self.active.lock().unwrap().drain(..) {
            s.stop();
        }
    }

    fn play_chord(&self, freqs: Vec<f64>, on_done: Box<dyn FnOnce() + Send + 'static>) {
        if freqs.is_empty() {
            on_done();
            return;
        }

        let sink = match Sink::try_new(&self.handle) {
            Ok(s) => Arc::new(s),
            Err(_) => { on_done(); return; }
        };

        if let Some(sf) = &self.sf {
            // SF2 path: render chord to WAV, decode via rodio.
            match render_chord_to_wav(sf, &freqs) {
                Some(wav) => {
                    match Decoder::new(Cursor::new(wav)) {
                        Ok(decoder) => {
                            sink.append(decoder);
                            let active = self.active.clone();
                            let sink_ref = sink.clone();
                            self.active.lock().unwrap().push(sink.clone());
                            std::thread::spawn(move || {
                                sink_ref.sleep_until_end();
                                active.lock().unwrap().retain(|s| !Arc::ptr_eq(s, &sink_ref));
                                on_done();
                            });
                            return;
                        }
                        Err(_) => {} // fall through to oscillator
                    }
                }
                None => {} // fall through to oscillator
            }
        }

        // Oscillator fallback: one sine wave per frequency with a 2s decay.
        let dur = Duration::from_secs_f32(2.0);
        let gain = 0.25_f32 / (freqs.len() as f32).sqrt();
        for hz in &freqs {
            let wave = rodio::source::SineWave::new(*hz as f32)
                .take_duration(dur)
                .fade_in(Duration::from_millis(15))
                .amplify(gain);
            sink.append(wave);
        }
        self.active.lock().unwrap().push(sink.clone());
        let active = self.active.clone();
        let sink_ref = sink.clone();
        std::thread::spawn(move || {
            std::thread::sleep(dur + Duration::from_millis(200));
            active.lock().unwrap().retain(|s| !Arc::ptr_eq(s, &sink_ref));
            on_done();
        });
    }

    fn metronome_start(&self, _bpm: f64, _beats_per_bar: u32) {}
    fn metronome_stop(&self) {}

    fn play_scale(
        &self,
        _freqs: Vec<f64>,
        _on_note: Box<dyn Fn(usize) + Send + 'static>,
        on_done: Box<dyn FnOnce() + Send + 'static>,
    ) {
        on_done();
    }

    fn play_timeline(
        &self,
        _timeline: NoteTimeline,
        _metronome: Option<(f64, u32)>,
        on_done: Box<dyn FnOnce() + Send + 'static>,
    ) {
        on_done();
    }

    fn play_wav(
        &self,
        _wav: Vec<u8>,
        _duration_s: f64,
        on_done: Box<dyn FnOnce() + Send + 'static>,
    ) {
        on_done();
    }
}

// ── SF2 rendering helpers ─────────────────────────────────────────────────────
// Adapted from app/dioxus_frontend/src/soundfont.rs

/// Render `freqs` as a simultaneous chord (2 s, GM program 24 = Acoustic Guitar Nylon).
fn render_chord_to_wav(sf: &Arc<SoundFont>, freqs: &[f64]) -> Option<Vec<u8>> {
    let timeline = NoteTimeline {
        events: freqs.iter().map(|&hz| crate::sequencer::NoteEvent {
            start_s: 0.0,
            duration_s: 2.0,
            freq_hz: hz,
            velocity: 0.85,
        }).collect(),
        total_duration_s: 2.5,
    };
    render_basic_timeline(sf, &timeline, 24)
}

/// Render a `NoteTimeline` with a single GM program to WAV bytes.
fn render_basic_timeline(sf: &Arc<SoundFont>, timeline: &NoteTimeline, program: u8) -> Option<Vec<u8>> {
    let settings = SynthesizerSettings::new(SAMPLE_RATE);
    let mut synth = Synthesizer::new(sf, &settings).ok()?;

    let total_samples = ((timeline.total_duration_s + 0.5) * SAMPLE_RATE as f64).ceil() as usize;
    let mut left = vec![0f32; total_samples];
    let mut right = vec![0f32; total_samples];

    // Sort events by start time.
    let mut sorted: Vec<_> = timeline.events.iter().collect();
    sorted.sort_by(|a, b| a.start_s.partial_cmp(&b.start_s).unwrap());

    #[derive(Debug)]
    enum Action {
        ProgramChange { channel: i32, program: i32 },
        PitchBend     { channel: i32, lsb: i32, msb: i32 },
        NoteOn        { channel: i32, key: i32, velocity: i32 },
        NoteOff       { channel: i32, key: i32 },
    }

    let melodic_chs: [i32; 15] = [0,1,2,3,4,5,6,7,8,10,11,12,13,14,15];
    let mut free_at = [0usize; 15];

    let mut schedule: Vec<(usize, Action)> = Vec::new();
    let mut channel_program = [255u8; 16];

    for ev in &sorted {
        let start_s = (ev.start_s * SAMPLE_RATE as f64).round() as usize;
        let end_s   = ((ev.start_s + ev.duration_s) * SAMPLE_RATE as f64).round() as usize;

        // Allocate a channel that is free at start_s.
        let best = free_at.iter().enumerate()
            .filter(|(_, &f)| f <= start_s)
            .min_by_key(|(_, &f)| f)
            .map(|(i, _)| i)
            .unwrap_or_else(|| free_at.iter().enumerate().min_by_key(|(_, &f)| f).map(|(i, _)| i).unwrap_or(0));
        free_at[best] = end_s;
        let ch = melodic_chs[best];

        if channel_program[ch as usize] != program {
            channel_program[ch as usize] = program;
            schedule.push((start_s, Action::ProgramChange { channel: ch, program: program as i32 }));
        }

        let (key, bend) = hz_to_midi_key(ev.freq_hz);
        let vel = (ev.velocity * 127.0).round() as i32;
        let (lsb, msb) = bend_to_midi(bend);
        schedule.push((start_s, Action::PitchBend { channel: ch, lsb, msb }));
        schedule.push((start_s, Action::NoteOn { channel: ch, key, velocity: vel }));
        schedule.push((end_s.min(total_samples), Action::NoteOff { channel: ch, key }));
    }

    schedule.sort_by(|a, b| {
        a.0.cmp(&b.0).then_with(|| {
            let order = |act: &Action| match act {
                Action::ProgramChange { .. } => 0,
                Action::PitchBend { .. }     => 1,
                Action::NoteOn { .. }        => 2,
                Action::NoteOff { .. }       => 3,
            };
            order(&a.1).cmp(&order(&b.1))
        })
    });

    let mut cursor = 0usize;
    let mut sched_idx = 0;

    while cursor < total_samples {
        let next = if sched_idx < schedule.len() { schedule[sched_idx].0 } else { total_samples };
        let render_to = next.min(total_samples);
        if render_to > cursor {
            synth.render(&mut left[cursor..render_to], &mut right[cursor..render_to]);
            cursor = render_to;
        }
        while sched_idx < schedule.len() && schedule[sched_idx].0 <= cursor {
            match &schedule[sched_idx].1 {
                Action::ProgramChange { channel, program } =>
                    synth.process_midi_message(*channel, 0xC0, *program, 0),
                Action::PitchBend { channel, lsb, msb } =>
                    synth.process_midi_message(*channel, 0xE0, *lsb, *msb),
                Action::NoteOn { channel, key, velocity } =>
                    synth.note_on(*channel, *key, *velocity),
                Action::NoteOff { channel, key } =>
                    synth.note_off(*channel, *key),
            }
            sched_idx += 1;
        }
    }

    Some(encode_wav(&left, &right, SAMPLE_RATE))
}

fn hz_to_midi_key(hz: f64) -> (i32, f64) {
    let exact = 69.0 + 12.0 * (hz / 440.0).log2();
    let key = exact.round() as i32;
    (key.clamp(0, 127), exact - key as f64)
}

fn bend_to_midi(semitones: f64) -> (i32, i32) {
    let value = ((semitones.clamp(-2.0, 2.0) / 2.0) * 8192.0 + 8192.0).round() as i32;
    let value = value.clamp(0, 16383);
    (value & 0x7F, (value >> 7) & 0x7F)
}

fn encode_wav(left: &[f32], right: &[f32], sample_rate: i32) -> Vec<u8> {
    let n = left.len();
    let ch: u16 = 2;
    let bps: u16 = 16;
    let byte_rate = sample_rate as u32 * ch as u32 * (bps / 8) as u32;
    let block_align = ch * (bps / 8);
    let data_size = (n * ch as usize * (bps / 8) as usize) as u32;

    let mut buf = Vec::with_capacity(44 + data_size as usize);
    buf.extend_from_slice(b"RIFF");
    buf.extend_from_slice(&(36 + data_size).to_le_bytes());
    buf.extend_from_slice(b"WAVE");
    buf.extend_from_slice(b"fmt ");
    buf.extend_from_slice(&16u32.to_le_bytes());
    buf.extend_from_slice(&1u16.to_le_bytes());
    buf.extend_from_slice(&ch.to_le_bytes());
    buf.extend_from_slice(&(sample_rate as u32).to_le_bytes());
    buf.extend_from_slice(&byte_rate.to_le_bytes());
    buf.extend_from_slice(&block_align.to_le_bytes());
    buf.extend_from_slice(&bps.to_le_bytes());
    buf.extend_from_slice(b"data");
    buf.extend_from_slice(&data_size.to_le_bytes());
    for i in 0..n {
        let l = (left[i].clamp(-1.0, 1.0) * 32767.0) as i16;
        let r = (right[i].clamp(-1.0, 1.0) * 32767.0) as i16;
        buf.extend_from_slice(&l.to_le_bytes());
        buf.extend_from_slice(&r.to_le_bytes());
    }
    buf
}

// ── Convenience: load SF2 from bytes ─────────────────────────────────────────

/// Parse a SoundFont from raw bytes. Returns `None` if invalid.
pub fn soundfont_from_bytes(bytes: &[u8]) -> Option<Arc<SoundFont>> {
    if bytes.len() < 12 || &bytes[0..4] != b"RIFF" {
        return None;
    }
    SoundFont::new(&mut Cursor::new(bytes)).ok().map(Arc::new)
}

// ── Thread-safe handle (for uniffi / Send + Sync contexts) ───────────────────

/// Commands sent to the audio thread.
enum AudioCmd {
    PlayChord(Vec<f64>),
    StopAll,
}

// SAFETY: Vec<f64> and the enum itself contain no raw pointers.
unsafe impl Send for AudioCmd {}

/// A `Send + Sync` handle to a `NativeAudioBackend` running on its own thread.
///
/// Use this instead of `NativeAudioBackend` directly when the owning struct must
/// implement `Send + Sync` (e.g. a uniffi Object).  `OutputStream` from cpal is
/// not `Send` on macOS/CoreAudio, so it must stay on its own thread.
pub struct NativeAudioHandle {
    tx: std::sync::mpsc::SyncSender<AudioCmd>,
}

// SAFETY: SyncSender<AudioCmd> is Send + Sync because AudioCmd is Send.
unsafe impl Sync for NativeAudioHandle {}

impl NativeAudioHandle {
    /// Spawn a dedicated audio thread, return a handle to it.
    /// Returns `None` if no audio device is available.
    pub fn spawn(sf: Option<Arc<SoundFont>>) -> Option<Self> {
        let (tx, rx) = std::sync::mpsc::sync_channel(16);
        std::thread::Builder::new()
            .name("xen-audio".into())
            .spawn(move || {
                let backend = match NativeAudioBackend::new(sf) {
                    Ok(b) => b,
                    Err(_) => return,
                };
                for cmd in rx {
                    match cmd {
                        AudioCmd::PlayChord(freqs) => {
                            backend.play_chord(freqs, Box::new(|| {}));
                        }
                        AudioCmd::StopAll => backend.stop_all(),
                    }
                }
            })
            .ok()?;
        Some(Self { tx })
    }

    pub fn play_chord(&self, freqs: Vec<f64>) {
        let _ = self.tx.try_send(AudioCmd::PlayChord(freqs));
    }

    pub fn stop_all(&self) {
        let _ = self.tx.try_send(AudioCmd::StopAll);
    }
}
