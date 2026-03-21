//! Real-time pitch tracking from microphone input.
//!
//! This module provides:
//! - A pluggable `PitchDetector` trait with two built-in implementations
//!   (YIN for monophonic, HPS for polyphonic).
//! - FFT utilities shared across detectors.
//! - Frequency-to-scale-degree mapping for arbitrary EDO temperaments.
//! - Browser microphone capture via the Web Audio API.

pub mod detector;
pub mod fft_util;
pub mod yin;
pub mod hps;
pub mod iterf0;
pub mod hz_to_degree;
pub mod mic_web;

pub use detector::{DetectedPitch, PitchDetector};
pub use yin::YinDetector;
pub use hps::HpsDetector;
pub use iterf0::IterF0Detector;
pub use hz_to_degree::{hz_to_scale_degree, hz_to_absolute_step};
pub use mic_web::{MicStream, MicEvent};
