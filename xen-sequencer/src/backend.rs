use crate::sequencer::NoteTimeline;

/// Platform-agnostic audio playback interface.
///
/// Implement this trait to integrate audio playback for a given platform
/// (Web Audio API, CoreAudio, ALSA, etc.). The Dioxus frontend uses
/// `WebAudioBackend` from this crate's `audio` module.
pub trait AudioBackend {
    /// Immediately silence all active audio.
    fn stop_all(&self);

    /// Start a repeating metronome click at the given BPM.
    /// `beats_per_bar` controls which beat gets the louder downbeat click.
    fn metronome_start(&self, bpm: f64, beats_per_bar: u32);

    /// Stop the standalone metronome.
    fn metronome_stop(&self);

    /// Play `freqs` as a sequentially ascending scale.
    /// `on_note` is called with each note's index as it starts.
    /// `on_done` is called when the last note has finished.
    fn play_scale(
        &self,
        freqs: Vec<f64>,
        on_note: Box<dyn Fn(usize) + 'static>,
        on_done: Box<dyn Fn() + 'static>,
    );

    /// Play `freqs` simultaneously as a chord.
    /// `on_done` is called when the chord fades out.
    fn play_chord(&self, freqs: Vec<f64>, on_done: Box<dyn Fn() + 'static>);

    /// Play a compiled `NoteTimeline`.
    /// Pass `metronome: Some((bpm, beats_per_bar))` to layer audible clicks.
    /// `on_done` is called when playback completes.
    fn play_timeline(
        &self,
        timeline: NoteTimeline,
        metronome: Option<(f64, u32)>,
        on_done: Box<dyn Fn() + 'static>,
    );

    /// Play pre-rendered WAV bytes.
    /// `on_done` is called when playback completes.
    fn play_wav(&self, wav: Vec<u8>, duration_s: f64, on_done: Box<dyn Fn() + 'static>);
}

// ── Web Audio implementation ──────────────────────────────────────────────────

/// `AudioBackend` implementation that drives playback via the browser Web Audio API.
/// Requires a Dioxus/WASM runtime environment.
pub struct WebAudioBackend;

impl AudioBackend for WebAudioBackend {
    fn stop_all(&self) {
        crate::audio::stop_all();
    }

    fn metronome_start(&self, bpm: f64, beats_per_bar: u32) {
        crate::audio::metronome_start(bpm, beats_per_bar);
    }

    fn metronome_stop(&self) {
        crate::audio::metronome_stop();
    }

    fn play_scale(
        &self,
        freqs: Vec<f64>,
        on_note: Box<dyn Fn(usize) + 'static>,
        on_done: Box<dyn Fn() + 'static>,
    ) {
        dioxus::prelude::spawn(async move {
            let mut pb = crate::audio::start_scale(&freqs);
            while let Some(idx) = pb.next_note().await {
                on_note(idx);
            }
            on_done();
        });
    }

    fn play_chord(&self, freqs: Vec<f64>, on_done: Box<dyn Fn() + 'static>) {
        dioxus::prelude::spawn(async move {
            crate::audio::play_chord(&freqs).await;
            on_done();
        });
    }

    fn play_timeline(
        &self,
        timeline: NoteTimeline,
        metronome: Option<(f64, u32)>,
        on_done: Box<dyn Fn() + 'static>,
    ) {
        dioxus::prelude::spawn(async move {
            crate::audio::play_timeline(&timeline, metronome).await;
            on_done();
        });
    }

    fn play_wav(&self, wav: Vec<u8>, duration_s: f64, on_done: Box<dyn Fn() + 'static>) {
        dioxus::prelude::spawn(async move {
            crate::audio::play_wav_bytes(&wav, duration_s).await;
            on_done();
        });
    }
}
