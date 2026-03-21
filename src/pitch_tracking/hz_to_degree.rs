//! Map detected frequencies to scale degrees within an arbitrary EDO temperament.

/// Map a detected frequency to the nearest scale degree.
///
/// Returns `Some(degree_index)` (0-based index into `scale_intervals`, where
/// 0 = root) if the pitch falls within `tolerance_cents` of a scale degree,
/// or `None` if it doesn't match any degree.
///
/// # Arguments
///
/// * `hz` — Detected fundamental frequency in Hz.
/// * `divisions` — Number of equal divisions of the period (e.g. 12 for 12-TET).
/// * `period` — Period ratio as (numerator, denominator), e.g. (2, 1) for octave.
/// * `root_hz` — Frequency of EDO step 0 (typically 440.0 for A4).
/// * `key` — EDO steps above step 0 where the scale root sits.
/// * `scale_intervals` — Intervals between consecutive scale degrees (in EDO steps).
/// * `tolerance_cents` — Maximum deviation in cents to accept a match.
pub fn hz_to_scale_degree(
    hz: f64,
    divisions: u32,
    period: (u32, u32),
    root_hz: f64,
    key: i32,
    scale_intervals: &[i32],
    tolerance_cents: f64,
) -> Option<usize> {
    if hz <= 0.0 || divisions == 0 || scale_intervals.is_empty() || root_hz <= 0.0 {
        return None;
    }

    let period_ratio = period.0 as f64 / period.1 as f64;
    if period_ratio <= 1.0 { return None; }

    let div = divisions as f64;
    let log_period = period_ratio.ln();

    // Continuous EDO step of the detected pitch (relative to step 0 at root_hz).
    let continuous_step = div * (hz / root_hz).ln() / log_period;

    // Shift by the key to get the step relative to the scale root.
    let relative_step = continuous_step - key as f64;

    // Size of one period in EDO steps (sum of all intervals).
    let period_size: i32 = scale_intervals.iter().sum();
    if period_size <= 0 { return None; }
    let period_f = period_size as f64;

    // Reduce to within one period (0..period_size).
    let pitch_class = ((relative_step % period_f) + period_f) % period_f;

    // Build cumulative scale degree positions: [0, i₀, i₀+i₁, ...].
    // The last entry equals period_size (which wraps to 0).
    let mut degree_positions = Vec::with_capacity(scale_intervals.len());
    let mut acc = 0i32;
    for &interval in scale_intervals {
        degree_positions.push(acc);
        acc += interval;
    }

    // Cents per EDO step.
    let cents_per_step = 1200.0 * log_period / (div * 2.0_f64.ln());
    let tolerance_steps = tolerance_cents / cents_per_step;

    // Find the closest scale degree.
    let mut best_degree: Option<usize> = None;
    let mut best_dist = f64::INFINITY;

    for (i, &pos) in degree_positions.iter().enumerate() {
        let pos_f = pos as f64;
        // Distance accounting for wraparound within the period.
        let d1 = (pitch_class - pos_f).abs();
        let d2 = (pitch_class - pos_f - period_f).abs();
        let d3 = (pitch_class - pos_f + period_f).abs();
        let dist = d1.min(d2).min(d3);
        if dist < best_dist {
            best_dist = dist;
            best_degree = Some(i);
        }
    }

    if best_dist <= tolerance_steps {
        best_degree
    } else {
        None
    }
}

/// Like [`hz_to_scale_degree`] but returns the **absolute EDO step** of the
/// matched degree rather than the degree index.  This allows octave-aware
/// highlighting: notes at the same pitch class in different octaves get
/// different step values and can be highlighted independently.
///
/// Returns `None` if no scale degree matches within `tolerance_cents`.
pub fn hz_to_absolute_step(
    hz: f64,
    divisions: u32,
    period: (u32, u32),
    root_hz: f64,
    key: i32,
    scale_intervals: &[i32],
    tolerance_cents: f64,
) -> Option<i32> {
    if hz <= 0.0 || divisions == 0 || scale_intervals.is_empty() || root_hz <= 0.0 {
        return None;
    }
    let period_ratio = period.0 as f64 / period.1 as f64;
    if period_ratio <= 1.0 { return None; }

    let div = divisions as f64;
    let log_period = period_ratio.ln();

    let continuous_step = div * (hz / root_hz).ln() / log_period;
    let relative_step = continuous_step - key as f64;

    let period_size: i32 = scale_intervals.iter().sum();
    if period_size <= 0 { return None; }
    let period_f = period_size as f64;

    let pitch_class = ((relative_step % period_f) + period_f) % period_f;

    // Build cumulative scale degree positions.
    let mut degree_positions = Vec::with_capacity(scale_intervals.len());
    let mut acc = 0i32;
    for &interval in scale_intervals {
        degree_positions.push(acc);
        acc += interval;
    }

    let cents_per_step = 1200.0 * log_period / (div * 2.0_f64.ln());
    let tolerance_steps = tolerance_cents / cents_per_step;

    let mut best_pos: Option<i32> = None;
    let mut best_dist = f64::INFINITY;

    for &pos in &degree_positions {
        let pos_f = pos as f64;
        let d1 = (pitch_class - pos_f).abs();
        let d2 = (pitch_class - pos_f - period_f).abs();
        let d3 = (pitch_class - pos_f + period_f).abs();
        let dist = d1.min(d2).min(d3);
        if dist < best_dist {
            best_dist = dist;
            best_pos = Some(pos);
        }
    }

    if best_dist > tolerance_steps { return None; }

    let pos = best_pos?;
    // Number of complete periods elapsed from key=0 to this note.
    let n_periods = ((relative_step - pos as f64) / period_f).round() as i32;
    Some(key + pos + n_periods * period_size)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_12tet_c_major() {
        // 12-TET, octave period, A4 = 440 Hz at step 0.
        // C major scale rooted at C (key = 3 steps above A): [2,2,1,2,2,2,1]
        let intervals = vec![2, 2, 1, 2, 2, 2, 1];
        let key = 3; // C is 3 semitones above A

        // C4 = 261.63 Hz → degree 0 (root)
        assert_eq!(hz_to_scale_degree(261.63, 12, (2, 1), 440.0, key, &intervals, 50.0), Some(0));

        // D4 = 293.66 Hz → degree 1
        assert_eq!(hz_to_scale_degree(293.66, 12, (2, 1), 440.0, key, &intervals, 50.0), Some(1));

        // E4 = 329.63 Hz → degree 2
        assert_eq!(hz_to_scale_degree(329.63, 12, (2, 1), 440.0, key, &intervals, 50.0), Some(2));

        // G4 = 392.00 Hz → degree 4
        assert_eq!(hz_to_scale_degree(392.00, 12, (2, 1), 440.0, key, &intervals, 50.0), Some(4));

        // C#4 = 277.18 Hz → not in C major scale with tight tolerance
        assert_eq!(hz_to_scale_degree(277.18, 12, (2, 1), 440.0, key, &intervals, 30.0), None);
    }

    #[test]
    fn test_19tet() {
        // 19-TET, key 0 (root at A4 = 440 Hz).
        // Major scale in 19-TET: [3, 3, 2, 3, 3, 3, 2]
        let intervals = vec![3, 3, 2, 3, 3, 3, 2];

        // Root at 440 Hz → degree 0
        assert_eq!(hz_to_scale_degree(440.0, 19, (2, 1), 440.0, 0, &intervals, 50.0), Some(0));

        // One octave up = 880 Hz → degree 0 (wraps around)
        assert_eq!(hz_to_scale_degree(880.0, 19, (2, 1), 440.0, 0, &intervals, 50.0), Some(0));
    }

    #[test]
    fn test_bohlen_pierce() {
        // Bohlen-Pierce: 13 divisions of tritave (3/1).
        // Lambda scale: [1, 2, 1, 2, 1, 2, 1, 2, 1]  (9 notes)
        let intervals = vec![1, 2, 1, 2, 1, 2, 1, 2, 1];

        // Root at 440 Hz → degree 0
        assert_eq!(hz_to_scale_degree(440.0, 13, (3, 1), 440.0, 0, &intervals, 50.0), Some(0));

        // One tritave up = 1320 Hz → degree 0
        assert_eq!(hz_to_scale_degree(1320.0, 13, (3, 1), 440.0, 0, &intervals, 50.0), Some(0));
    }

    #[test]
    fn test_out_of_tolerance() {
        // 12-TET chromatic scale (all semitones): [1,1,1,1,1,1,1,1,1,1,1,1]
        let chromatic: Vec<i32> = vec![1; 12];
        // With tolerance of 1 cent, even a small detuning should fail.
        // A4 + 25 cents ≈ 446.4 Hz — halfway between A and A#.
        let detuned = 440.0 * 2.0_f64.powf(25.0 / 1200.0);
        assert_eq!(hz_to_scale_degree(detuned, 12, (2, 1), 440.0, 0, &chromatic, 1.0), None);
    }
}
