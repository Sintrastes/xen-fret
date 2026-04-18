//! Harmonic Product Spectrum (HPS) polyphonic pitch detector.
//!
//! HPS multiplies progressively downsampled copies of the magnitude spectrum
//! to reinforce frequencies whose harmonics are all present.  Peaks in the
//! product identify fundamental frequencies even when multiple notes sound
//! simultaneously.

use crate::detector::{DetectedPitch, PitchDetector};
use crate::fft_util::{apply_hann_window, rms_db, MagnitudeState};

pub struct HpsDetector {
    /// Number of harmonic copies to multiply (2..=8 is typical).
    num_harmonics: usize,
    min_hz: f64,
    max_hz: f64,
    /// Minimum peak height (relative to the maximum) to consider a detection.
    peak_threshold: f64,
    silence_threshold_db: f64,
    mag_state: Option<MagnitudeState>,
    windowed: Vec<f64>,
    /// Reusable buffer for the harmonic product.
    product: Vec<f64>,
}

impl HpsDetector {
    pub fn new(num_harmonics: usize, min_hz: f64, max_hz: f64, peak_threshold: f64) -> Self {
        Self {
            num_harmonics: num_harmonics.max(2),
            min_hz,
            max_hz,
            peak_threshold,
            silence_threshold_db: -50.0,
            mag_state: None,
            windowed: Vec::new(),
            product: Vec::new(),
        }
    }

    fn ensure_state(&mut self, n: usize) {
        if self.windowed.len() != n {
            self.mag_state = Some(MagnitudeState::new(n));
            self.windowed.resize(n, 0.0);
        }
    }
}

impl PitchDetector for HpsDetector {
    fn detect(&mut self, samples: &[f32], sample_rate: u32) -> Vec<DetectedPitch> {
        let n = samples.len();
        if n < 8 { return vec![]; }

        let amp_db = rms_db(samples);
        if amp_db < self.silence_threshold_db {
            return vec![];
        }

        self.ensure_state(n);
        apply_hann_window(samples, &mut self.windowed);

        let mag = self.mag_state.as_mut().unwrap().magnitude_spectrum(&self.windowed);
        let spec_len = mag.len(); // n/2 + 1

        let sr = sample_rate as f64;
        let bin_hz = sr / n as f64;

        // The product spectrum is only valid up to spec_len / num_harmonics bins,
        // since the highest harmonic copy downsamples by that factor.
        let product_len = spec_len / self.num_harmonics;
        if product_len == 0 { return vec![]; }

        self.product.resize(product_len, 0.0);

        // Initialize with the first (un-downsampled) copy.
        self.product[..product_len].copy_from_slice(&mag[..product_len]);

        // Multiply by downsampled copies.
        for h in 2..=self.num_harmonics {
            for k in 0..product_len {
                let idx = k * h;
                if idx < spec_len {
                    self.product[k] *= mag[idx];
                } else {
                    self.product[k] = 0.0;
                }
            }
        }

        // Bin range for the search.
        let k_min = ((self.min_hz / bin_hz).ceil() as usize).max(1);
        let k_max = ((self.max_hz / bin_hz).floor() as usize).min(product_len - 1);
        if k_min >= k_max { return vec![]; }

        // Find the global maximum for normalization.
        let global_max = self.product[k_min..=k_max]
            .iter()
            .copied()
            .fold(0.0_f64, f64::max);
        if global_max < 1e-20 { return vec![]; }

        // Peak detection: find local maxima above the threshold.
        let abs_thresh = global_max * self.peak_threshold;
        let mut peaks: Vec<(usize, f64)> = Vec::new();

        for k in k_min..=k_max {
            let v = self.product[k];
            if v < abs_thresh { continue; }
            let left  = if k > 0 { self.product[k - 1] } else { 0.0 };
            let right = if k + 1 < product_len { self.product[k + 1] } else { 0.0 };
            if v >= left && v >= right {
                peaks.push((k, v));
            }
        }

        // Convert peaks to DetectedPitch with parabolic interpolation.
        let mut result: Vec<DetectedPitch> = peaks.iter().filter_map(|&(k, _)| {
            let refined_k = if k > k_min && k < k_max {
                let s0 = self.product[k - 1].ln().max(-300.0);
                let s1 = self.product[k].ln().max(-300.0);
                let s2 = self.product[k + 1].ln().max(-300.0);
                let denom = 2.0 * s1 - s0 - s2;
                if denom.abs() > 1e-12 {
                    k as f64 + (s0 - s2) / (2.0 * denom)
                } else {
                    k as f64
                }
            } else {
                k as f64
            };

            let hz = refined_k * bin_hz;
            if hz < self.min_hz || hz > self.max_hz { return None; }

            let confidence = (self.product[k] / global_max).sqrt().min(1.0);
            Some(DetectedPitch { hz, confidence, amplitude_db: amp_db })
        }).collect();

        // Sort by confidence descending.
        result.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());
        result
    }

    fn name(&self) -> &str { "HPS" }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::PI;

    /// Generate a signal with harmonics (more realistic for HPS which relies on
    /// harmonic series reinforcement).
    fn harmonic_signal(fundamental: f32, num_harmonics: usize, sample_rate: u32, n: usize) -> Vec<f32> {
        let mut buf = vec![0.0f32; n];
        for h in 1..=num_harmonics {
            let amp = 1.0 / h as f32;  // natural harmonic decay
            let hz = fundamental * h as f32;
            for i in 0..n {
                buf[i] += amp * (2.0 * PI * hz * i as f32 / sample_rate as f32).sin();
            }
        }
        let peak = buf.iter().map(|s| s.abs()).fold(0.0f32, f32::max);
        if peak > 0.0 { for s in &mut buf { *s /= peak; } }
        buf
    }

    fn mixed_harmonic_signal(fundamentals: &[f32], num_harmonics: usize, sample_rate: u32, n: usize) -> Vec<f32> {
        let mut buf = vec![0.0f32; n];
        for &fund in fundamentals {
            for h in 1..=num_harmonics {
                let amp = 1.0 / h as f32;
                let hz = fund * h as f32;
                for i in 0..n {
                    buf[i] += amp * (2.0 * PI * hz * i as f32 / sample_rate as f32).sin();
                }
            }
        }
        let peak = buf.iter().map(|s| s.abs()).fold(0.0f32, f32::max);
        if peak > 0.0 { for s in &mut buf { *s /= peak; } }
        buf
    }

    #[test]
    fn detect_single_pitch() {
        // HPS needs harmonics to reinforce the fundamental.
        // Larger FFT (8192) gives ~5.9 Hz resolution for better accuracy.
        let samples = harmonic_signal(440.0, 6, 48000, 8192);
        let mut hps = HpsDetector::new(5, 50.0, 4000.0, 0.1);
        let result = hps.detect(&samples, 48000);
        assert!(!result.is_empty(), "HPS should detect at least one pitch");
        let detected = result[0].hz;
        let error_cents = 1200.0 * (detected / 440.0).log2().abs();
        assert!(error_cents < 50.0, "Expected ~440 Hz, got {detected:.2} Hz ({error_cents:.1} cents off)");
    }

    #[test]
    fn detect_two_pitches() {
        // A3 (220 Hz) + E4 (329.63 Hz) — well-separated fundamentals.
        // Both with harmonics for HPS to work.  Larger buffer for resolution.
        let samples = mixed_harmonic_signal(&[220.0, 329.63], 6, 48000, 16384);
        let mut hps = HpsDetector::new(3, 100.0, 2000.0, 0.02);
        let result = hps.detect(&samples, 48000);
        let found_a3 = result.iter().any(|p| (1200.0 * (p.hz / 220.0).log2().abs()) < 50.0);
        let found_e4 = result.iter().any(|p| (1200.0 * (p.hz / 329.63).log2().abs()) < 50.0);
        assert!(found_a3, "Should detect A3 (220 Hz), got: {:?}", result.iter().map(|p| p.hz).collect::<Vec<_>>());
        assert!(found_e4, "Should detect E4 (329.63 Hz), got: {:?}", result.iter().map(|p| p.hz).collect::<Vec<_>>());
    }

    #[test]
    fn silence_returns_empty() {
        let samples = vec![0.0f32; 4096];
        let mut hps = HpsDetector::new(5, 50.0, 4000.0, 0.1);
        let result = hps.detect(&samples, 48000);
        assert!(result.is_empty());
    }
}
