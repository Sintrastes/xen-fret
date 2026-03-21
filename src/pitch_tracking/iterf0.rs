//! Iterative F0 polyphonic pitch detector.
//!
//! Detects multiple simultaneous pitches by iteratively finding the dominant
//! fundamental frequency via harmonic scoring, then subtracting its harmonic
//! energy from the spectrum before searching for the next pitch.

use crate::pitch_tracking::detector::{DetectedPitch, PitchDetector};
use crate::pitch_tracking::fft_util::{apply_hann_window, rms_db, MagnitudeState};

pub struct IterF0Detector {
    max_voices: usize,
    num_harmonics: usize,
    min_hz: f64,
    max_hz: f64,
    harmonic_threshold: f64,
    silence_threshold_db: f64,
    subtraction_width: usize,
    mag_state: Option<MagnitudeState>,
    windowed: Vec<f64>,
    working_spectrum: Vec<f64>,
}

impl IterF0Detector {
    pub fn new(
        max_voices: usize,
        num_harmonics: usize,
        min_hz: f64,
        max_hz: f64,
        harmonic_threshold: f64,
    ) -> Self {
        Self {
            max_voices: max_voices.max(1),
            num_harmonics: num_harmonics.max(2),
            min_hz,
            max_hz,
            harmonic_threshold,
            silence_threshold_db: -50.0,
            subtraction_width: 3,
            mag_state: None,
            windowed: Vec::new(),
            working_spectrum: Vec::new(),
        }
    }

    fn ensure_state(&mut self, n: usize) {
        if self.windowed.len() != n {
            self.mag_state = Some(MagnitudeState::new(n));
            self.windowed.resize(n, 0.0);
        }
    }

    /// Score a candidate F0 at bin `k` by summing weighted harmonic energy.
    /// Returns a score normalized by the total weight and `norm` (typically the
    /// max bin value).
    fn harmonic_score(
        spectrum: &[f64],
        k: usize,
        spec_len: usize,
        norm: f64,
        num_harmonics: usize,
    ) -> f64 {
        if norm < 1e-20 || k >= spec_len {
            return 0.0;
        }
        let fundamental_mag = spectrum[k];
        // The candidate's own bin must have meaningful energy.
        if fundamental_mag < norm * 0.02 {
            return 0.0;
        }

        let mut score = 0.0;
        let mut total_weight = 0.0;
        for h in 1..=num_harmonics {
            let idx = k * h;
            if idx >= spec_len {
                break;
            }
            // Weight lower harmonics more heavily for octave disambiguation.
            let weight = 1.0 / h as f64;
            score += weight * spectrum[idx];
            total_weight += weight;
        }
        if total_weight < 1e-12 {
            return 0.0;
        }
        let harmonic_avg = score / (total_weight * norm);

        // Soft boost for candidates where the fundamental itself is present.
        // Using sqrt so that a fundamental at 25% of max still retains 50% of
        // the score — important for guitar where upper harmonics are often
        // louder than the fundamental.
        let fundamental_ratio = (fundamental_mag / norm).sqrt();
        harmonic_avg * fundamental_ratio
    }

    /// Zero out bins around each harmonic of F0 at bin `k`.
    fn subtract_harmonics(
        spectrum: &mut [f64],
        k: usize,
        num_harmonics: usize,
        subtraction_width: usize,
    ) {
        let spec_len = spectrum.len();
        for h in 1..=num_harmonics {
            let center = k * h;
            if center >= spec_len {
                break;
            }
            let lo = center.saturating_sub(subtraction_width);
            let hi = (center + subtraction_width + 1).min(spec_len);
            for bin in lo..hi {
                spectrum[bin] = 0.0;
            }
        }
    }
}

impl PitchDetector for IterF0Detector {
    fn detect(&mut self, samples: &[f32], sample_rate: u32) -> Vec<DetectedPitch> {
        let n = samples.len();
        if n < 8 {
            return vec![];
        }

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

        let k_min = ((self.min_hz / bin_hz).ceil() as usize).max(1);
        let k_max = ((self.max_hz / bin_hz).floor() as usize).min(spec_len - 1);
        if k_min >= k_max {
            return vec![];
        }

        // Copy magnitude spectrum into working buffer.
        self.working_spectrum.resize(spec_len, 0.0);
        self.working_spectrum.copy_from_slice(mag);

        // Original global max — used for confidence so that later iterations
        // (which operate on a residual spectrum) produce lower confidence values.
        let original_max = self.working_spectrum[k_min..=k_max]
            .iter()
            .copied()
            .fold(0.0_f64, f64::max);
        if original_max < 1e-20 {
            return vec![];
        }

        let mut results: Vec<DetectedPitch> = Vec::new();

        for _ in 0..self.max_voices {
            // Find the current iteration's max for peak selection.
            let iter_max = self.working_spectrum[k_min..=k_max]
                .iter()
                .copied()
                .fold(0.0_f64, f64::max);
            if iter_max < 1e-20 {
                break;
            }

            // Find all local maxima (peaks) in the working spectrum.
            let mut best_k: usize = 0;
            let mut best_score: f64 = -1.0;

            for k in k_min..=k_max {
                let v = self.working_spectrum[k];
                // Must be above a noise floor relative to this iteration's max.
                if v < iter_max * 0.05 {
                    continue;
                }
                // Local maximum check.
                let left = if k > 0 {
                    self.working_spectrum[k - 1]
                } else {
                    0.0
                };
                let right = if k + 1 < spec_len {
                    self.working_spectrum[k + 1]
                } else {
                    0.0
                };
                if v < left || v < right {
                    continue;
                }

                let score =
                    Self::harmonic_score(&self.working_spectrum, k, spec_len, iter_max, self.num_harmonics);
                if score > best_score {
                    best_score = score;
                    best_k = k;
                }
            }

            if best_score < self.harmonic_threshold {
                break;
            }

            // Parabolic interpolation for sub-bin accuracy.
            let refined_k = if best_k > k_min && best_k < k_max {
                let s0 = self.working_spectrum[best_k - 1];
                let s1 = self.working_spectrum[best_k];
                let s2 = self.working_spectrum[best_k + 1];
                let denom = 2.0 * s1 - s0 - s2;
                if denom.abs() > 1e-12 {
                    best_k as f64 + (s0 - s2) / (2.0 * denom)
                } else {
                    best_k as f64
                }
            } else {
                best_k as f64
            };

            let hz = refined_k * bin_hz;
            // Confidence: scale by how strong this peak is relative to the
            // original (un-subtracted) spectrum.  First detection gets high
            // confidence; later detections from residual energy get less.
            let peak_mag = self.working_spectrum[best_k];
            let confidence = (best_score * (peak_mag / original_max)).min(1.0);
            if hz >= self.min_hz && hz <= self.max_hz {
                results.push(DetectedPitch {
                    hz,
                    confidence,
                    amplitude_db: amp_db,
                });
            }

            // Subtract this F0's harmonics from the working spectrum.
            Self::subtract_harmonics(
                &mut self.working_spectrum,
                best_k,
                self.num_harmonics,
                self.subtraction_width,
            );
        }

        // Sort by confidence descending.
        results.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());
        results
    }

    fn name(&self) -> &str {
        "IterF0"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::PI;

    fn harmonic_signal(
        fundamental: f32,
        num_harmonics: usize,
        sample_rate: u32,
        n: usize,
    ) -> Vec<f32> {
        let mut buf = vec![0.0f32; n];
        for h in 1..=num_harmonics {
            let amp = 1.0 / h as f32;
            let hz = fundamental * h as f32;
            for i in 0..n {
                buf[i] += amp * (2.0 * PI * hz * i as f32 / sample_rate as f32).sin();
            }
        }
        let peak = buf.iter().map(|s| s.abs()).fold(0.0f32, f32::max);
        if peak > 0.0 {
            for s in &mut buf {
                *s /= peak;
            }
        }
        buf
    }

    fn mixed_harmonic_signal(
        fundamentals: &[f32],
        num_harmonics: usize,
        sample_rate: u32,
        n: usize,
    ) -> Vec<f32> {
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
        if peak > 0.0 {
            for s in &mut buf {
                *s /= peak;
            }
        }
        buf
    }

    #[test]
    fn detect_single_harmonic_pitch() {
        let samples = harmonic_signal(440.0, 6, 48000, 8192);
        let mut det = IterF0Detector::new(6, 8, 70.0, 2000.0, 0.3);
        let result = det.detect(&samples, 48000);
        assert!(!result.is_empty(), "Should detect at least one pitch");
        let detected = result[0].hz;
        let error_cents = 1200.0 * (detected / 440.0).log2().abs();
        assert!(
            error_cents < 20.0,
            "Expected ~440 Hz, got {detected:.2} Hz ({error_cents:.1} cents off)"
        );
    }

    #[test]
    fn detect_two_pitches() {
        // A3 (220 Hz) + E4 (329.63 Hz) — well-separated fundamentals.
        let samples = mixed_harmonic_signal(&[220.0, 329.63], 6, 48000, 16384);
        let mut det = IterF0Detector::new(6, 8, 70.0, 2000.0, 0.2);
        let result = det.detect(&samples, 48000);
        let found_a3 = result
            .iter()
            .any(|p| (1200.0 * (p.hz / 220.0).log2().abs()) < 50.0);
        let found_e4 = result
            .iter()
            .any(|p| (1200.0 * (p.hz / 329.63).log2().abs()) < 50.0);
        assert!(
            found_a3,
            "Should detect A3 (220 Hz), got: {:?}",
            result.iter().map(|p| p.hz).collect::<Vec<_>>()
        );
        assert!(
            found_e4,
            "Should detect E4 (329.63 Hz), got: {:?}",
            result.iter().map(|p| p.hz).collect::<Vec<_>>()
        );
    }

    #[test]
    fn detect_chord() {
        // C major triad: C4 (261.63), E4 (329.63), G4 (392.00)
        let samples = mixed_harmonic_signal(&[261.63, 329.63, 392.00], 6, 48000, 16384);
        let mut det = IterF0Detector::new(6, 8, 70.0, 2000.0, 0.15);
        let result = det.detect(&samples, 48000);
        let found_c = result
            .iter()
            .any(|p| (1200.0 * (p.hz / 261.63).log2().abs()) < 50.0);
        let found_e = result
            .iter()
            .any(|p| (1200.0 * (p.hz / 329.63).log2().abs()) < 50.0);
        let found_g = result
            .iter()
            .any(|p| (1200.0 * (p.hz / 392.00).log2().abs()) < 50.0);
        assert!(
            found_c,
            "Should detect C4 (261.63 Hz), got: {:?}",
            result.iter().map(|p| p.hz).collect::<Vec<_>>()
        );
        assert!(
            found_e,
            "Should detect E4 (329.63 Hz), got: {:?}",
            result.iter().map(|p| p.hz).collect::<Vec<_>>()
        );
        assert!(
            found_g,
            "Should detect G4 (392.00 Hz), got: {:?}",
            result.iter().map(|p| p.hz).collect::<Vec<_>>()
        );
    }

    #[test]
    fn silence_returns_empty() {
        let samples = vec![0.0f32; 4096];
        let mut det = IterF0Detector::new(6, 8, 70.0, 2000.0, 0.3);
        let result = det.detect(&samples, 48000);
        assert!(result.is_empty());
    }

    #[test]
    fn pure_sine_detected() {
        // A pure sine has only a fundamental — should still be detected since
        // h=1 contributes to the score.
        let samples: Vec<f32> = (0..8192)
            .map(|i| (2.0 * PI * 440.0 * i as f32 / 48000.0).sin())
            .collect();
        let mut det = IterF0Detector::new(6, 8, 70.0, 2000.0, 0.1);
        let result = det.detect(&samples, 48000);
        assert!(!result.is_empty(), "Should detect pure sine");
        let error_cents = 1200.0 * (result[0].hz / 440.0).log2().abs();
        assert!(
            error_cents < 20.0,
            "Expected ~440 Hz, got {:.2} Hz ({error_cents:.1} cents off)",
            result[0].hz
        );
    }
}
