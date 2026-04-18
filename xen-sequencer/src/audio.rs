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

// Timeline playback: arbitrary (start, duration, freq, velocity) tuples.
const PLAY_TIMELINE_JS: &str = r#"(async () => {
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

  const events = [__EVENTS__];
  const keys    = Object.keys(S.bufs).map(Number);
  const hasBufs = keys.length > 0;

  events.forEach(([start, dur, hz, vel]) => {
    const t = ctx.currentTime + 0.05 + start;

    if (hasBufs) {
      const targetMidi = 69 + 12 * Math.log2(hz / 440);
      const nearest = keys.reduce((a, b) =>
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
      gain.gain.linearRampToValueAtTime(vel * 0.85, t + 0.008);
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
      gain.gain.linearRampToValueAtTime(vel * 0.28, t + 0.015);
      gain.gain.linearRampToValueAtTime(0, t + dur);
      osc.start(t);
      osc.stop(t + dur + 0.01);
    }
  });

  // Metronome clicks: array of [time_s, is_downbeat].
  const metro = [__METRONOME__];
  metro.forEach(([time, down]) => {
    const t = ctx.currentTime + 0.05 + time;
    const osc  = ctx.createOscillator();
    const gain = ctx.createGain();
    osc.connect(gain);
    gain.connect(ctx.destination);
    osc.type = 'sine';
    osc.frequency.value = down ? 1200 : 800;
    gain.gain.setValueAtTime(0, t);
    gain.gain.linearRampToValueAtTime(down ? 0.35 : 0.22, t + 0.002);
    gain.gain.exponentialRampToValueAtTime(0.001, t + 0.06);
    osc.start(t);
    osc.stop(t + 0.07);
  });

  const totalMs = Math.round((__TOTAL_DUR__ + 0.3) * 1000);
  setTimeout(() => dioxus.send('done'), totalMs);
})();
"#;

// ── Standalone metronome ────────────────────────────────────────────────────

const METRONOME_START_JS: &str = r#"(async () => {
  if (!window._xfSampler) {
    window._xfSampler = { ctx: null, bufs: {}, loading: null, ready: false };
  }
  const S = window._xfSampler;
  if (!S.ctx) S.ctx = new (window.AudioContext || window.webkitAudioContext)();
  await S.ctx.resume();
  const ctx = S.ctx;

  // Stop any existing metronome before starting.
  if (window._xfMetro && window._xfMetro.timer) {
    clearInterval(window._xfMetro.timer);
  }

  const bpm = __BPM__;
  const beatSec = 60.0 / bpm;
  const schedAhead = 0.15;
  let nextClick = ctx.currentTime + 0.05;

  function scheduleClick(t, down) {
    const osc  = ctx.createOscillator();
    const gain = ctx.createGain();
    osc.connect(gain);
    gain.connect(ctx.destination);
    osc.type = 'sine';
    osc.frequency.value = down ? 1200 : 800;
    gain.gain.setValueAtTime(0, t);
    gain.gain.linearRampToValueAtTime(down ? 0.35 : 0.22, t + 0.002);
    gain.gain.exponentialRampToValueAtTime(0.001, t + 0.06);
    osc.start(t);
    osc.stop(t + 0.07);
  }

  let beat = 0;
  const beatsPerBar = __BEATS_PER_BAR__;
  window._xfMetro = {
    timer: setInterval(() => {
      while (nextClick < ctx.currentTime + schedAhead) {
        scheduleClick(nextClick, beat % beatsPerBar === 0);
        beat++;
        nextClick += beatSec;
      }
    }, 25),
  };
})();
"#;

const METRONOME_STOP_JS: &str = r#"(function() {
  if (window._xfMetro && window._xfMetro.timer) {
    clearInterval(window._xfMetro.timer);
    window._xfMetro.timer = null;
  }
})();
"#;

/// Start a standalone metronome click track at the given BPM.
/// `beats_per_bar` controls which beat gets the higher-pitched downbeat click.
pub fn metronome_start(bpm: f64, beats_per_bar: u32) {
    let js = METRONOME_START_JS
        .replace("__BPM__", &format!("{bpm:.2}"))
        .replace("__BEATS_PER_BAR__", &beats_per_bar.to_string());
    let _ = eval(&js);
}

/// Stop the standalone metronome.
pub fn metronome_stop() {
    let _ = eval(METRONOME_STOP_JS);
}

// ── WAV playback ────────────────────────────────────────────────────────────
// Plays pre-rendered WAV audio (from soundfont synthesis) via Web Audio API.

const PLAY_WAV_JS: &str = r#"(async () => {
  if (!window._xfSampler) {
    window._xfSampler = { ctx: null, bufs: {}, loading: null, ready: false };
  }
  const S = window._xfSampler;
  if (!S.ctx) S.ctx = new (window.AudioContext || window.webkitAudioContext)();
  const ctx = S.ctx;
  await ctx.resume();

  // Decode base64 WAV to ArrayBuffer.
  const b64 = '__WAV_B64__';
  const bin = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);

  const audioBuf = await ctx.decodeAudioData(bytes.buffer);
  const src = ctx.createBufferSource();
  src.buffer = audioBuf;
  src.connect(ctx.destination);
  src.start(0);

  const totalMs = Math.round((__TOTAL_DUR__ + 0.3) * 1000);
  setTimeout(() => dioxus.send('done'), totalMs);
})();
"#;

// ── Stop ─────────────────────────────────────────────────────────────────────

const STOP_JS: &str = r#"(function() {
  const S = window._xfSampler;
  if (S && S.ctx) {
    S.ctx.close();
    S.ctx = null;
    S.ready = false;
    S.loading = null;
  }
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

/// Play a pre-compiled `NoteTimeline` (from the sequencer).
///
/// If `metronome` is `Some((bpm, beats_per_bar))`, audible clicks are added on
/// each beat (higher pitch on the downbeat).
pub async fn play_timeline(
    timeline: &crate::sequencer::NoteTimeline,
    metronome: Option<(f64, u32)>,
) {
    if timeline.events.is_empty() { return; }
    let events_js = timeline.events.iter()
        .map(|e| format!("[{:.4},{:.4},{:.4},{:.4}]", e.start_s, e.duration_s, e.freq_hz, e.velocity))
        .collect::<Vec<_>>()
        .join(",");

    let metro_js = match metronome {
        Some((bpm, beats_per_bar)) => {
            let beat_s = 60.0 / bpm;
            let total_beats = (timeline.total_duration_s / beat_s).ceil() as u32;
            (0..total_beats)
                .map(|i| {
                    let t = i as f64 * beat_s;
                    let downbeat = (i % beats_per_bar) == 0;
                    format!("[{:.4},{}]", t, if downbeat { "1" } else { "0" })
                })
                .collect::<Vec<_>>()
                .join(",")
        }
        None => String::new(),
    };

    let js = PLAY_TIMELINE_JS
        .replace("__EVENTS__", &events_js)
        .replace("__METRONOME__", &metro_js)
        .replace("__TOTAL_DUR__", &format!("{:.4}", timeline.total_duration_s));
    let mut ev = eval(&js);
    let _ = ev.recv::<String>().await;
}

/// Play pre-rendered WAV bytes via Web Audio API.
/// The WAV data is base64-encoded and decoded in JS.
pub async fn play_wav_bytes(wav: &[u8], total_duration_s: f64) {
    // Base64-encode the WAV data.
    let b64 = base64_encode(wav);
    let js = PLAY_WAV_JS
        .replace("__WAV_B64__", &b64)
        .replace("__TOTAL_DUR__", &format!("{:.4}", total_duration_s));
    let mut ev = eval(&js);
    let _ = ev.recv::<String>().await;
}

/// Immediately silence all audio by closing the AudioContext.
/// A fresh context (and re-cached samples) will be created on the next play.
pub fn stop_all() {
    let _ = eval(STOP_JS);
}

fn base64_encode(data: &[u8]) -> String {
    const CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity((data.len() + 2) / 3 * 4);
    for chunk in data.chunks(3) {
        let b0 = chunk[0] as u32;
        let b1 = if chunk.len() > 1 { chunk[1] as u32 } else { 0 };
        let b2 = if chunk.len() > 2 { chunk[2] as u32 } else { 0 };
        let triple = (b0 << 16) | (b1 << 8) | b2;
        out.push(CHARS[((triple >> 18) & 0x3F) as usize] as char);
        out.push(CHARS[((triple >> 12) & 0x3F) as usize] as char);
        if chunk.len() > 1 {
            out.push(CHARS[((triple >> 6) & 0x3F) as usize] as char);
        } else {
            out.push('=');
        }
        if chunk.len() > 2 {
            out.push(CHARS[(triple & 0x3F) as usize] as char);
        } else {
            out.push('=');
        }
    }
    out
}
