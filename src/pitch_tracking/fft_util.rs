//! FFT utilities shared by pitch detection algorithms.
//!
//! All state structs cache their `RealFftPlanner` and scratch buffers so that
//! repeated calls (at ~30 fps) avoid re-allocation.

use realfft::{RealFftPlanner, RealToComplex, ComplexToReal};
use rustfft::num_complex::Complex;
use std::f64::consts::PI;
use std::sync::Arc;

// ── Hann window ──────────────────────────────────────────────────────────────

/// Apply a Hann window to `input` (f32 PCM), writing the result into `output`
/// (f64 for FFT precision).  `output.len()` must equal `input.len()`.
pub fn apply_hann_window(input: &[f32], output: &mut [f64]) {
    let n = input.len();
    assert_eq!(n, output.len());
    let inv = 2.0 * PI / n as f64;
    for i in 0..n {
        let w = 0.5 * (1.0 - (inv * i as f64).cos());
        output[i] = input[i] as f64 * w;
    }
}

// ── Autocorrelation via FFT ──────────────────────────────────────────────────

/// Cached state for FFT-accelerated autocorrelation.
pub struct AutocorrelationState {
    n: usize,
    // Padded length (next power-of-two ≥ 2N) for linear (non-circular) correlation.
    padded: usize,
    fwd: Arc<dyn RealToComplex<f64>>,
    inv: Arc<dyn ComplexToReal<f64>>,
    freq_buf: Vec<Complex<f64>>,
    time_buf: Vec<f64>,
    scratch_fwd: Vec<Complex<f64>>,
    scratch_inv: Vec<Complex<f64>>,
    result: Vec<f64>,
}

impl AutocorrelationState {
    pub fn new(n: usize) -> Self {
        let padded = (2 * n).next_power_of_two();
        let mut planner = RealFftPlanner::new();
        let fwd = planner.plan_fft_forward(padded);
        let inv = planner.plan_fft_inverse(padded);
        let freq_buf = vec![Complex::default(); padded / 2 + 1];
        let time_buf = vec![0.0; padded];
        let scratch_fwd = vec![Complex::default(); fwd.get_scratch_len()];
        let scratch_inv = vec![Complex::default(); inv.get_scratch_len()];
        let result = vec![0.0; n];
        Self { n, padded, fwd, inv, freq_buf, time_buf, scratch_fwd, scratch_inv, result }
    }

    /// Compute the (un-normalized) autocorrelation of `windowed` (length == `n`).
    /// Returns a slice of length `n` where index τ is the autocorrelation at lag τ.
    pub fn autocorrelation(&mut self, windowed: &[f64]) -> &[f64] {
        assert_eq!(windowed.len(), self.n);

        // Zero-pad into time_buf.
        self.time_buf[..self.n].copy_from_slice(windowed);
        for v in &mut self.time_buf[self.n..] { *v = 0.0; }

        // Forward FFT.
        self.fwd.process_with_scratch(&mut self.time_buf, &mut self.freq_buf, &mut self.scratch_fwd)
            .expect("forward FFT failed");

        // Power spectrum in-place: X[k] = |X[k]|².
        for c in &mut self.freq_buf {
            *c = Complex::new(c.norm_sqr(), 0.0);
        }

        // Inverse FFT → autocorrelation.
        self.inv.process_with_scratch(&mut self.freq_buf, &mut self.time_buf, &mut self.scratch_inv)
            .expect("inverse FFT failed");

        // Normalize by padded length (realfft convention) and copy first n lags.
        let scale = 1.0 / self.padded as f64;
        for i in 0..self.n {
            self.result[i] = self.time_buf[i] * scale;
        }
        &self.result
    }
}

// ── Magnitude spectrum via FFT ───────────────────────────────────────────────

/// Cached state for computing the magnitude spectrum of a real signal.
pub struct MagnitudeState {
    n: usize,
    fwd: Arc<dyn RealToComplex<f64>>,
    freq_buf: Vec<Complex<f64>>,
    time_buf: Vec<f64>,
    scratch: Vec<Complex<f64>>,
    magnitude: Vec<f64>,
}

impl MagnitudeState {
    pub fn new(n: usize) -> Self {
        let mut planner = RealFftPlanner::new();
        let fwd = planner.plan_fft_forward(n);
        let freq_buf = vec![Complex::default(); n / 2 + 1];
        let time_buf = vec![0.0; n];
        let scratch = vec![Complex::default(); fwd.get_scratch_len()];
        let magnitude = vec![0.0; n / 2 + 1];
        Self { n, fwd, freq_buf, time_buf, scratch, magnitude }
    }

    /// Compute the magnitude spectrum of `windowed` (length == `n`).
    /// Returns a slice of length `n/2 + 1` where index k corresponds to
    /// frequency `k * sample_rate / n`.
    pub fn magnitude_spectrum(&mut self, windowed: &[f64]) -> &[f64] {
        assert_eq!(windowed.len(), self.n);

        self.time_buf.copy_from_slice(windowed);
        self.fwd.process_with_scratch(&mut self.time_buf, &mut self.freq_buf, &mut self.scratch)
            .expect("forward FFT failed");

        for (i, c) in self.freq_buf.iter().enumerate() {
            self.magnitude[i] = c.norm();
        }
        &self.magnitude
    }
}

// ── Amplitude measurement ────────────────────────────────────────────────────

/// RMS amplitude of a signal in dB (relative to full-scale = 1.0).
pub fn rms_db(samples: &[f32]) -> f64 {
    if samples.is_empty() { return -f64::INFINITY; }
    let sum_sq: f64 = samples.iter().map(|&s| (s as f64) * (s as f64)).sum();
    let rms = (sum_sq / samples.len() as f64).sqrt();
    if rms < 1e-12 { -120.0 } else { 20.0 * rms.log10() }
}
