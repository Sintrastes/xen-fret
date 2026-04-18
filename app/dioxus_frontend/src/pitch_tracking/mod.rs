//! Real-time pitch tracking from microphone input.
//!
//! DSP algorithms and Hz-to-degree math live in `xen_dsp`.
//! This module re-exports them alongside the browser mic capture adapter.

pub mod mic_web;

pub use xen_dsp::detector::{DetectedPitch, PitchDetector};
pub use xen_dsp::yin::YinDetector;
pub use xen_dsp::hps::HpsDetector;
pub use xen_dsp::iterf0::IterF0Detector;
pub use xen_dsp::pitch_tracking::{hz_to_scale_degree, hz_to_absolute_step};
pub use mic_web::{MicStream, MicEvent};
