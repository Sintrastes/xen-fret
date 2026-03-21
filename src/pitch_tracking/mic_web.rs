//! Browser microphone capture via the Web Audio API.
//!
//! Follows the same `eval()` + `dioxus.send()` pattern established in
//! `audio.rs`.  The JS side opens the mic, connects an AnalyserNode, and
//! streams raw PCM samples back to Rust at ~30 fps via `requestAnimationFrame`.

use dioxus::document::eval;
use serde::Deserialize;

// ── JS code ──────────────────────────────────────────────────────────────────
// __FFT_SIZE__ is replaced at call time with the desired AnalyserNode buffer size.

const MIC_START_JS: &str = r#"(async () => {
  try {
    const stream = await navigator.mediaDevices.getUserMedia({
      audio: { echoCancellation: false, noiseSuppression: false, autoGainControl: false }
    });
    const ctx = new (window.AudioContext || window.webkitAudioContext)();
    await ctx.resume();
    const source = ctx.createMediaStreamSource(stream);
    const analyser = ctx.createAnalyser();
    analyser.fftSize = __FFT_SIZE__;
    source.connect(analyser);

    const buf = new Float32Array(analyser.fftSize);
    window._xfMicStop = false;
    window._xfMicStream = stream;
    window._xfMicCtx = ctx;

    dioxus.send(JSON.stringify({ type: "started", rate: ctx.sampleRate }));

    function poll() {
      if (window._xfMicStop) {
        stream.getTracks().forEach(t => t.stop());
        ctx.close();
        dioxus.send(JSON.stringify({ type: "stopped" }));
        return;
      }
      analyser.getFloatTimeDomainData(buf);
      // Silence gate: only send frames with meaningful signal.
      let peak = 0;
      for (let i = 0; i < buf.length; i++) {
        const a = Math.abs(buf[i]);
        if (a > peak) peak = a;
      }
      if (peak > 0.005) {
        dioxus.send(JSON.stringify({
          type: "samples",
          rate: ctx.sampleRate,
          data: Array.from(buf)
        }));
      }
      requestAnimationFrame(poll);
    }
    requestAnimationFrame(poll);
  } catch (e) {
    dioxus.send(JSON.stringify({ type: "error", msg: e.message || String(e) }));
  }
})();
"#;

/// JS snippet to stop the mic listener.  Safe to call even if no listener is active.
pub const MIC_STOP_JS: &str = "if (typeof window._xfMicStop !== 'undefined') { window._xfMicStop = true; }";

// ── Rust API ─────────────────────────────────────────────────────────────────

/// Deserialization target for JSON messages from the JS mic listener.
#[derive(Deserialize)]
#[serde(tag = "type")]
enum RawMicMessage {
    #[serde(rename = "started")]
    Started { rate: u32 },
    #[serde(rename = "samples")]
    Samples { rate: u32, data: Vec<f32> },
    #[serde(rename = "stopped")]
    Stopped,
    #[serde(rename = "error")]
    Error { msg: String },
}

/// Events produced by the microphone stream.
#[derive(Debug)]
pub enum MicEvent {
    /// Mic successfully opened.  `sample_rate` is the actual hardware rate.
    Started { sample_rate: u32 },
    /// A chunk of raw PCM samples from the AnalyserNode.
    Samples { rate: u32, data: Vec<f32> },
    /// Mic was stopped (by calling `stop()`).
    Stopped,
    /// An error occurred (e.g. permission denied).
    Error(String),
}

/// A live microphone stream.  Created with `MicStream::start()`, polled with
/// `next_event()`, and terminated with `stop()`.
pub struct MicStream {
    ev: dioxus::document::Eval,
}

impl MicStream {
    /// Request mic permission and begin streaming PCM chunks.
    ///
    /// `fft_size` controls the AnalyserNode buffer size (must be a power of 2,
    /// typically 2048 or 4096).
    pub fn start(fft_size: usize) -> Self {
        let js = MIC_START_JS.replace("__FFT_SIZE__", &fft_size.to_string());
        Self { ev: eval(&js) }
    }

    /// Receive the next event from the mic.  Returns `None` if the JS channel
    /// is closed unexpectedly.
    pub async fn next_event(&mut self) -> Option<MicEvent> {
        let raw: String = match self.ev.recv::<String>().await {
            Ok(s) => s,
            Err(_) => return None,
        };
        match serde_json::from_str::<RawMicMessage>(&raw) {
            Ok(RawMicMessage::Started { rate }) => Some(MicEvent::Started { sample_rate: rate }),
            Ok(RawMicMessage::Samples { rate, data }) => Some(MicEvent::Samples { rate, data }),
            Ok(RawMicMessage::Stopped) => Some(MicEvent::Stopped),
            Ok(RawMicMessage::Error { msg }) => Some(MicEvent::Error(msg)),
            Err(e) => Some(MicEvent::Error(format!("JSON parse error: {e}"))),
        }
    }

    /// Signal the JS side to stop the mic and release the stream.
    pub fn stop(&self) {
        let _ = eval(MIC_STOP_JS);
    }
}
