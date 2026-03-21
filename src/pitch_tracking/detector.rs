//! Pitch detector trait and common types.

/// A single detected pitch with confidence and amplitude metadata.
#[derive(Debug, Clone)]
pub struct DetectedPitch {
    /// Fundamental frequency in Hz.
    pub hz: f64,
    /// Detection confidence in 0.0..=1.0 (higher = more certain).
    pub confidence: f64,
    /// Signal amplitude in dB (useful for gating quiet/noisy frames).
    pub amplitude_db: f64,
}

/// Pluggable pitch detection backend.
///
/// Monophonic detectors return 0 or 1 results; polyphonic detectors may
/// return multiple.  Callers treat the result uniformly as a `Vec`.
pub trait PitchDetector: Send {
    /// Analyze a buffer of mono f32 PCM samples at the given sample rate.
    /// Returns detected pitches sorted by confidence (descending).
    fn detect(&mut self, samples: &[f32], sample_rate: u32) -> Vec<DetectedPitch>;

    /// Human-readable name of this detector (e.g. "YIN", "HPS").
    fn name(&self) -> &str;
}
