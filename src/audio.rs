//! Browser audio playback via the Web Audio API.
//!
//! On the first play, acoustic guitar soundfont samples are fetched from
//! gleitz/midi-js-soundfonts and cached in `window._xfSampler`.  Subsequent
//! plays are instant.  If sample loading fails for any reason the engine falls
//! back to triangle-wave oscillators so playback always works.
//!
//! Any arbitrary Hz value is handled correctly: the nearest sample is selected
//! and its `playbackRate` is adjusted to hit the exact target frequency.  This
//! means microtonal scales work without any rounding to 12-TET.

use dioxus::document::eval;

// ── JS audio engine ───────────────────────────────────────────────────────────
//
// __FREQS__ is replaced with the Hz array literal at call time.
// Using str::replace avoids the need to escape every JS brace in a format!().

const PLAY_JS: &str = r#"(async () => {
  // Singleton sampler: persists across calls so samples are only fetched once.
  if (!window._xfSampler) {
    window._xfSampler = { ctx: null, bufs: {}, loading: null, ready: false };
  }
  const S = window._xfSampler;

  if (!S.ctx) S.ctx = new (window.AudioContext || window.webkitAudioContext)();
  const ctx = S.ctx;
  await ctx.resume();

  // Load acoustic guitar samples on first play; subsequent calls await the
  // already-resolved promise and continue immediately.
  if (!S.ready) {
    if (!S.loading) {
      const BASE  = 'https://gleitz.github.io/midi-js-soundfonts/MusyngKite/acoustic_guitar_nylon-mp3/';
      const NAMES = ['C','Cs','D','Ds','E','F','Fs','G','Gs','A','As','B'];
      // One sample every 4 semitones from E2–C6; max pitch-shift is ±2 semitones.
      S.loading = Promise.all(
        [40,44,48,52,56,60,64,68,72,76,80,84].map(async (m) => {
          const name = NAMES[m % 12];
          const oct  = Math.floor(m / 12) - 1;
          try {
            const resp = await fetch(BASE + name + oct + '.mp3');
            S.bufs[m] = await ctx.decodeAudioData(await resp.arrayBuffer());
          } catch (_) {}
        })
      ).then(() => { S.ready = true; });
    }
    await S.loading;
  }

  const freqs   = [__FREQS__];
  const dur     = 0.65;  // seconds per note
  const step    = 0.72;  // note onset interval (dur + gap)
  const keys    = Object.keys(S.bufs).map(Number);
  const hasBufs = keys.length > 0;

  // Notify Rust which note index is starting so it can highlight the fretboard.
  freqs.forEach((_, i) => {
    setTimeout(() => dioxus.send('n:' + i), 50 + Math.round(i * step * 1000));
  });

  freqs.forEach((hz, i) => {
    const t = ctx.currentTime + 0.05 + i * step;

    if (hasBufs) {
      // Soundfont path: pick the nearest cached sample, shift pitch via playbackRate.
      const targetMidi = 69 + 12 * Math.log2(hz / 440);
      const nearest    = keys.reduce((a, b) =>
        Math.abs(b - targetMidi) < Math.abs(a - targetMidi) ? b : a
      );
      const sampleHz = 440 * Math.pow(2, (nearest - 69) / 12);

      const src  = ctx.createBufferSource();
      const gain = ctx.createGain();
      src.buffer = S.bufs[nearest];
      src.playbackRate.value = hz / sampleHz;
      src.connect(gain);
      gain.connect(ctx.destination);
      // Pluck envelope: near-instant attack, slow natural decay.
      gain.gain.setValueAtTime(0, t);
      gain.gain.linearRampToValueAtTime(0.85, t + 0.008);
      gain.gain.linearRampToValueAtTime(0, t + dur);
      src.start(t);
      src.stop(t + dur + 0.05);
    } else {
      // Oscillator fallback (no network required).
      const osc  = ctx.createOscillator();
      const gain = ctx.createGain();
      osc.connect(gain);
      gain.connect(ctx.destination);
      osc.type = 'triangle';
      osc.frequency.value = hz;
      gain.gain.setValueAtTime(0, t);
      gain.gain.linearRampToValueAtTime(0.28, t + 0.015);
      gain.gain.setValueAtTime(0.28, t + dur * 0.72);
      gain.gain.linearRampToValueAtTime(0, t + dur);
      osc.start(t);
      osc.stop(t + dur + 0.01);
    }
  });

  // Signal Rust that playback is finished.
  const finishMs = Math.round((freqs.length * step + 0.2) * 1000);
  setTimeout(() => dioxus.send('done'), finishMs);
})();
"#;

// All notes start simultaneously; longer sustain for a chord voicing.
const PLAY_CHORD_JS: &str = r#"(async () => {
  if (!window._xfSampler) {
    window._xfSampler = { ctx: null, bufs: {}, loading: null, ready: false };
  }
  const S = window._xfSampler;

  if (!S.ctx) S.ctx = new (window.AudioContext || window.webkitAudioContext)();
  const ctx = S.ctx;
  await ctx.resume();

  if (!S.ready) {
    if (!S.loading) {
      const BASE  = 'https://gleitz.github.io/midi-js-soundfonts/MusyngKite/acoustic_guitar_nylon-mp3/';
      const NAMES = ['C','Cs','D','Ds','E','F','Fs','G','Gs','A','As','B'];
      S.loading = Promise.all(
        [40,44,48,52,56,60,64,68,72,76,80,84].map(async (m) => {
          const name = NAMES[m % 12];
          const oct  = Math.floor(m / 12) - 1;
          try {
            const resp = await fetch(BASE + name + oct + '.mp3');
            S.bufs[m] = await ctx.decodeAudioData(await resp.arrayBuffer());
          } catch (_) {}
        })
      ).then(() => { S.ready = true; });
    }
    await S.loading;
  }

  const freqs   = [__FREQS__];
  const dur     = 2.0;   // longer sustain for chords
  const keys    = Object.keys(S.bufs).map(Number);
  const hasBufs = keys.length > 0;
  // Per-voice gain so simultaneous notes don't clip.
  const voiceGain = Math.min(0.85, 0.85 / Math.sqrt(freqs.length));

  freqs.forEach((hz) => {
    const t = ctx.currentTime + 0.05;  // all voices at the same instant

    if (hasBufs) {
      const targetMidi = 69 + 12 * Math.log2(hz / 440);
      const nearest    = keys.reduce((a, b) =>
        Math.abs(b - targetMidi) < Math.abs(a - targetMidi) ? b : a
      );
      const sampleHz = 440 * Math.pow(2, (nearest - 69) / 12);

      const src  = ctx.createBufferSource();
      const gain = ctx.createGain();
      src.buffer = S.bufs[nearest];
      src.playbackRate.value = hz / sampleHz;
      src.connect(gain);
      gain.connect(ctx.destination);
      gain.gain.setValueAtTime(0, t);
      gain.gain.linearRampToValueAtTime(voiceGain, t + 0.008);
      gain.gain.linearRampToValueAtTime(0, t + dur);
      src.start(t);
      src.stop(t + dur + 0.05);
    } else {
      const osc  = ctx.createOscillator();
      const gain = ctx.createGain();
      osc.connect(gain);
      gain.connect(ctx.destination);
      osc.type = 'triangle';
      osc.frequency.value = hz;
      gain.gain.setValueAtTime(0, t);
      gain.gain.linearRampToValueAtTime(0.28 / Math.sqrt(freqs.length), t + 0.015);
      gain.gain.linearRampToValueAtTime(0, t + dur);
      osc.start(t);
      osc.stop(t + dur + 0.01);
    }
  });

  const finishMs = Math.round((dur + 0.3) * 1000);
  setTimeout(() => dioxus.send('done'), finishMs);
})();
"#;

// ── Public API ────────────────────────────────────────────────────────────────

/// A live scale playback session.  Poll `next_note()` to receive each note
/// onset index as it fires, or `None` when the scale has finished.
pub struct ScalePlayback {
    ev: dioxus::document::Eval,
}

impl ScalePlayback {
    /// Returns `Some(note_index)` as each note begins, `None` when done.
    pub async fn next_note(&mut self) -> Option<usize> {
        loop {
            match self.ev.recv::<String>().await {
                Ok(s) if s == "done" => return None,
                Ok(s) if s.starts_with("n:") => {
                    return s[2..].parse().ok();
                }
                Ok(_) => continue, // unexpected message, skip
                Err(_) => return None,
            }
        }
    }
}

/// Start playing `freqs` as a scale and return a handle to track note onsets.
pub fn start_scale(freqs: &[f64]) -> ScalePlayback {
    let freq_js = freqs.iter().map(|f| format!("{f:.4}")).collect::<Vec<_>>().join(",");
    let js = PLAY_JS.replace("__FREQS__", &freq_js);
    ScalePlayback { ev: eval(&js) }
}

/// Play `freqs` (Hz) as an ascending scale.  Returns when the last note ends.
pub async fn play_scale(freqs: &[f64]) {
    if freqs.is_empty() { return; }
    let mut pb = start_scale(freqs);
    while pb.next_note().await.is_some() {}
}

/// Play `freqs` (Hz) all simultaneously as a chord.  Returns when the chord fades.
pub async fn play_chord(freqs: &[f64]) {
    if freqs.is_empty() { return; }
    let freq_js = freqs.iter().map(|f| format!("{f:.4}")).collect::<Vec<_>>().join(",");
    let js = PLAY_CHORD_JS.replace("__FREQS__", &freq_js);
    let mut ev = eval(&js);
    let _ = ev.recv::<String>().await;
}
