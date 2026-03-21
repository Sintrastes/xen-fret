//! YIN monophonic pitch detector.
//!
//! Implements the YIN algorithm (de Cheveigné & Kawahara, 2002) with
//! FFT-accelerated autocorrelation for O(N log N) performance.

use crate::pitch_tracking::detector::{DetectedPitch, PitchDetector};
use crate::pitch_tracking::fft_util::{apply_hann_window, rms_db, AutocorrelationState};

pub struct YinDetector {
    /// Absolute threshold for the cumulative mean normalized difference.
    /// Lower values are stricter (fewer false positives, more missed notes).
    /// Typical range: 0.05–0.20.  Default: 0.15.
    threshold: f64,
    min_hz: f64,
    max_hz: f64,
    /// Minimum amplitude (dB) to attempt detection.  Frames below this are
    /// treated as silence.
    silence_threshold_db: f64,
    /// Cached FFT state (lazily initialized on first call for the given size).
    acf_state: Option<AutocorrelationState>,
    windowed: Vec<f64>,
}

impl YinDetector {
    pub fn new(threshold: f64, min_hz: f64, max_hz: f64) -> Self {
        Self {
            threshold,
            min_hz,
            max_hz,
            silence_threshold_db: -50.0,
            acf_state: None,
            windowed: Vec::new(),
        }
    }

    /// Ensure internal buffers are sized for `n` samples.
    fn ensure_state(&mut self, n: usize) {
        let need_new = self.acf_state.as_ref().map_or(true, |_| {
            self.windowed.len() != n
        });
        if need_new {
            self.acf_state = Some(AutocorrelationState::new(n));
            self.windowed.resize(n, 0.0);
        }
    }
}

impl PitchDetector for YinDetector {
    fn detect(&mut self, samples: &[f32], sample_rate: u32) -> Vec<DetectedPitch> {
        let n = samples.len();
        if n < 4 { return vec![]; }

        let amp_db = rms_db(samples);
        if amp_db < self.silence_threshold_db {
            return vec![];
        }

        self.ensure_state(n);
        apply_hann_window(samples, &mut self.windowed);

        let acf = self.acf_state.as_mut().unwrap();
        let r = acf.autocorrelation(&self.windowed);
        // r[0] is the energy (autocorrelation at lag 0).

        let sr = sample_rate as f64;
        let tau_min = (sr / self.max_hz).ceil() as usize;
        let tau_max = ((sr / self.min_hz).floor() as usize).min(n / 2);
        if tau_min >= tau_max || tau_max >= n {
            return vec![];
        }

        // ── Step 1–2: Difference function d(τ) and CMNDF d'(τ) ──────────────
        // d(τ) = r[0] − r[τ]  (for a zero-mean signal, the standard YIN
        // difference function simplifies to this via the autocorrelation).
        // But the full definition is: d(τ) = Σ (x[j] - x[j+τ])²
        // which equals 2·(r[0] - r[τ]) when using the unwindowed signal.
        // We compute it properly from the autocorrelation:
        //   d(τ) = r[0] + r_shifted[0] - 2·r[τ]
        // For a windowed signal the energy at lag 0 is r[0], and we approximate
        // r_shifted[0] ≈ r[0] for small τ (valid when τ << N).

        let energy = r[0];
        if energy < 1e-12 { return vec![]; }

        // Cumulative mean normalized difference d'(τ).
        // d'(0) = 1 by definition, d'(τ) = d(τ) / [(1/τ) Σ_{j=1}^{τ} d(j)]
        let mut cmndf = vec![0.0f64; tau_max + 1];
        cmndf[0] = 1.0;
        let mut running_sum = 0.0;
        for tau in 1..=tau_max {
            let d_tau = 2.0 * (energy - r[tau.min(r.len() - 1)]);
            let d_tau = d_tau.max(0.0);
            running_sum += d_tau;
            cmndf[tau] = if running_sum < 1e-12 { 1.0 } else { d_tau * tau as f64 / running_sum };
        }

        // ── Step 3: Absolute threshold search ───────────────────────────────
        // Find the first τ ≥ tau_min where cmndf[τ] < threshold and
        // cmndf[τ] is a local minimum.
        let mut best_tau: Option<usize> = None;
        for tau in tau_min..tau_max {
            if cmndf[tau] < self.threshold {
                // Walk forward to the local minimum.
                let mut t = tau;
                while t + 1 <= tau_max && cmndf[t + 1] < cmndf[t] {
                    t += 1;
                }
                best_tau = Some(t);
                break;
            }
        }

        // Fallback: if no tau passed the threshold, pick the global minimum
        // in the search range.
        let best_tau = best_tau.unwrap_or_else(|| {
            (tau_min..=tau_max)
                .min_by(|&a, &b| cmndf[a].partial_cmp(&cmndf[b]).unwrap())
                .unwrap_or(tau_min)
        });

        let confidence = 1.0 - cmndf[best_tau].min(1.0);

        // ── Step 4: Parabolic interpolation ──────────────────────────────────
        let refined_tau = if best_tau > tau_min && best_tau < tau_max {
            let s0 = cmndf[best_tau - 1];
            let s1 = cmndf[best_tau];
            let s2 = cmndf[best_tau + 1];
            let denom = 2.0 * s1 - s0 - s2;
            if denom.abs() > 1e-12 {
                best_tau as f64 + (s0 - s2) / (2.0 * denom)
            } else {
                best_tau as f64
            }
        } else {
            best_tau as f64
        };

        let hz = sr / refined_tau;
        if hz < self.min_hz || hz > self.max_hz {
            return vec![];
        }

        vec![DetectedPitch { hz, confidence, amplitude_db: amp_db }]
    }

    fn name(&self) -> &str { "YIN" }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::PI;

    fn sine_wave(hz: f32, sample_rate: u32, n: usize) -> Vec<f32> {
        (0..n).map(|i| (2.0 * PI * hz * i as f32 / sample_rate as f32).sin()).collect()
    }

    #[test]
    fn detect_a4() {
        let samples = sine_wave(440.0, 48000, 2048);
        let mut yin = YinDetector::new(0.15, 50.0, 4000.0);
        let result = yin.detect(&samples, 48000);
        assert_eq!(result.len(), 1);
        let detected = result[0].hz;
        let error_cents = 1200.0 * (detected / 440.0).log2().abs();
        assert!(error_cents < 5.0, "Expected ~440 Hz, got {detected:.2} Hz ({error_cents:.1} cents off)");
    }

    #[test]
    fn detect_low_e() {
        // Low E needs a larger buffer for good resolution at ~82 Hz.
        let samples = sine_wave(82.41, 48000, 8192);
        let mut yin = YinDetector::new(0.15, 50.0, 4000.0);
        let result = yin.detect(&samples, 48000);
        assert_eq!(result.len(), 1);
        let detected = result[0].hz;
        let error_cents = 1200.0 * (detected / 82.41).log2().abs();
        assert!(error_cents < 20.0, "Expected ~82.41 Hz, got {detected:.2} Hz ({error_cents:.1} cents off)");
    }

    #[test]
    fn silence_returns_empty() {
        let samples = vec![0.0f32; 2048];
        let mut yin = YinDetector::new(0.15, 50.0, 4000.0);
        let result = yin.detect(&samples, 48000);
        assert!(result.is_empty());
    }
}
