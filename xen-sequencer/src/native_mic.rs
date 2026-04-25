//! Native microphone capture using cpal.
//!
//! `NativeMicStream` — direct use (for Linux/GTK where cpal::Stream is Send, or
//! inside dedicated worker threads on macOS/Android).
//!
//! `NativeMicHandle` — Send + Sync wrapper that spawns an owner thread, for use
//! in contexts that require Send + Sync (e.g. uniffi Objects on macOS/Android).

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use std::sync::mpsc::{channel, Receiver};
use std::sync::Mutex;

use crate::mic::MicEvent;

// ── NativeMicStream ───────────────────────────────────────────────────────────

/// A live microphone stream that delivers `MicEvent::Samples` frames.
///
/// `cpal::Stream` is `!Send` on macOS/CoreAudio — do not move this struct across
/// threads on macOS.  Use `NativeMicHandle` in those contexts.
pub struct NativeMicStream {
    _stream: cpal::Stream,
    rx: Receiver<MicEvent>,
    sample_rate: u32,
}

impl NativeMicStream {
    /// Open the default input device, begin streaming mono `f32` frames of
    /// `target_frame_size` samples each.  Silence-gates frames with peak < 0.005,
    /// matching the behaviour of the web `AnalyserNode` backend.
    pub fn start(target_frame_size: usize) -> Result<Self, String> {
        let host = cpal::default_host();
        let device = host
            .default_input_device()
            .ok_or_else(|| "no default input device".to_string())?;
        let config = device
            .default_input_config()
            .map_err(|e| e.to_string())?;
        let sample_rate = config.sample_rate().0;
        let channels = config.channels() as usize;

        let (tx, rx) = channel();
        // Signal "started" immediately so the receiver doesn't block forever.
        tx.send(MicEvent::Started { sample_rate }).ok();

        let tx_cb = tx.clone();
        let mut buffer: Vec<f32> = Vec::with_capacity(target_frame_size);

        let stream = device
            .build_input_stream(
                &config.into(),
                move |data: &[f32], _| {
                    for frame in data.chunks(channels) {
                        let mono = frame.iter().sum::<f32>() / channels as f32;
                        buffer.push(mono);
                        if buffer.len() >= target_frame_size {
                            let chunk = std::mem::replace(
                                &mut buffer,
                                Vec::with_capacity(target_frame_size),
                            );
                            // Silence gate matching mic_web.rs threshold.
                            let peak = chunk.iter().fold(0.0_f32, |m, &x| m.max(x.abs()));
                            if peak > 0.005 {
                                let _ = tx_cb.send(MicEvent::Samples {
                                    rate: sample_rate,
                                    data: chunk,
                                });
                            }
                        }
                    }
                },
                |err| eprintln!("xen-mic cpal error: {err}"),
                None,
            )
            .map_err(|e| e.to_string())?;

        stream.play().map_err(|e| e.to_string())?;
        Ok(Self { _stream: stream, rx, sample_rate })
    }

    /// Blocking receive.  Returns `None` when the channel is closed.
    pub fn next_event(&self) -> Option<MicEvent> {
        self.rx.recv().ok()
    }

    /// Non-blocking receive.  Returns `None` when no event is ready.
    pub fn try_next_event(&self) -> Option<MicEvent> {
        self.rx.try_recv().ok()
    }

    pub fn sample_rate(&self) -> u32 {
        self.sample_rate
    }
}

// ── NativeMicHandle ───────────────────────────────────────────────────────────

/// A `Send + Sync` handle to a `NativeMicStream` running on its own thread.
///
/// Use this instead of `NativeMicStream` directly when the owning struct must
/// implement `Send + Sync` (e.g. a uniffi Object on macOS/Android).
pub struct NativeMicHandle {
    stop_tx: std::sync::mpsc::SyncSender<()>,
    events: Mutex<std::sync::mpsc::Receiver<MicEvent>>,
}

// SAFETY: stop_tx is SyncSender (Send+Sync); events is guarded by Mutex.
// MicEvent contains no raw pointers.
unsafe impl Sync for NativeMicHandle {}

impl NativeMicHandle {
    /// Spawn a dedicated thread that owns the cpal stream and forwards `MicEvent`s.
    /// Returns `None` if the thread cannot be spawned.
    pub fn spawn(target_frame_size: usize) -> Option<Self> {
        let (stop_tx, stop_rx) = std::sync::mpsc::sync_channel::<()>(1);
        let (ev_tx, ev_rx) = std::sync::mpsc::channel::<MicEvent>();

        std::thread::Builder::new()
            .name("xen-mic".into())
            .spawn(move || {
                let stream = match NativeMicStream::start(target_frame_size) {
                    Ok(s) => s,
                    Err(msg) => {
                        let _ = ev_tx.send(MicEvent::Error(msg));
                        return;
                    }
                };
                // Forward frames from the cpal callback channel to the external channel
                // until a Stop signal arrives.
                loop {
                    if stop_rx.try_recv().is_ok() {
                        break;
                    }
                    match stream.try_next_event() {
                        Some(ev) => {
                            if ev_tx.send(ev).is_err() {
                                break;
                            }
                        }
                        None => std::thread::sleep(std::time::Duration::from_millis(5)),
                    }
                }
                let _ = ev_tx.send(MicEvent::Stopped);
                // `stream` dropped here — releases the cpal device on this thread.
            })
            .ok()?;

        Some(Self {
            stop_tx,
            events: Mutex::new(ev_rx),
        })
    }

    /// Non-blocking receive.
    pub fn try_next_event(&self) -> Option<MicEvent> {
        self.events.lock().unwrap().try_recv().ok()
    }

    /// Blocking receive.  Returns `None` when the channel is closed.
    pub fn next_event_blocking(&self) -> Option<MicEvent> {
        self.events.lock().unwrap().recv().ok()
    }

    /// Signal the owner thread to stop capturing.
    pub fn stop(&self) {
        let _ = self.stop_tx.try_send(());
    }
}
