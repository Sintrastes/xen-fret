///
/// Determines whether an accidental symbol appears before or after the natural
///  when modifying a note name.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AccidentalPosition {
    Prefix,
    Postfix,
}

/// A named modifier that shifts a natural note by some number of EDO steps.
/// The offset may be negative (e.g. flat = −1 in most systems, but +1 in
/// 16-TET harmonic notation where sharp and flat are inverted).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Accidental {
    /// Display symbol, e.g. "\u{E262}" (♯), "\u{E260}" (♭), "^", "\\"
    pub name: String,
    /// EDO-step offset (can be negative)
    pub offset: i32,
    pub position: AccidentalPosition,
}

/// A named note without any accidental, located at a specific EDO step.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Natural {
    pub name: String,
    pub degree: u32,
}

/// A complete notation system: a set of naturals plus a set of accidentals
/// that can modify them.  Together they generate names for all EDO degrees.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NotationSystem {
    #[cfg_attr(feature = "serde", serde(default))]
    pub temperament_name: String,
    pub name: String,
    pub naturals: Vec<Natural>,
    /// Accidentals available in this system, in definition order.
    pub accidentals: Vec<Accidental>,
}

impl NotationSystem {
    // ── Primitives ────────────────────────────────────────────────────────

    /// Format a (natural, optional accidental) pair into a display string.
    pub fn format_note(&self, nat: &Natural, acc: Option<&Accidental>) -> String {
        match acc {
            None => nat.name.clone(),
            Some(a) => match a.position {
                AccidentalPosition::Prefix => format!("{}{}", a.name, nat.name),
                AccidentalPosition::Postfix => format!("{}{}", nat.name, a.name),
            },
        }
    }

    /// Compute the EDO degree that (natural + optional accidental) resolves to.
    pub fn resolve_degree(&self, nat: &Natural, acc: Option<&Accidental>, edo: u32) -> u32 {
        (nat.degree as i32 + acc.map_or(0, |a| a.offset)).rem_euclid(edo as i32) as u32
    }

    // ── Name lookup ───────────────────────────────────────────────────────

    /// Return the best name for `degree`, biased toward the given key's
    /// accidental direction.
    ///
    /// Scoring (lower = preferred):
    ///   - If `degree` == the key's resolved degree → always return the key's
    ///     own spelling (overrides everything else).
    ///   - Plain natural with no accidental → score 0.
    ///   - Accidental whose sign matches the key direction → score `|offset|`
    ///     (smaller accidentals preferred within the same direction).
    ///   - Accidental whose sign opposes the key direction → score `100 + |offset|`.
    ///   - Ties are broken deterministically by (natural index, accidental index).
    ///
    /// When `key_natural_idx` / `key_accidental_idx` are both `None` the
    /// function falls back to a neutral ordering: naturals first, then
    /// accidentals ordered by `|offset|` (sharps and flats equally weighted).
    pub fn name_for_degree_biased(
        &self,
        degree: u32,
        edo: u32,
        key_natural_idx: Option<usize>,
        key_accidental_idx: Option<usize>,
    ) -> Option<String> {
        let key_nat = key_natural_idx.and_then(|ni| self.naturals.get(ni));
        let key_acc = key_accidental_idx.and_then(|ai| self.accidentals.get(ai));

        // Rule 1: key's own spelling takes absolute priority.
        if let Some(nat) = key_nat {
            if self.resolve_degree(nat, key_acc, edo) == degree {
                return Some(self.format_note(nat, key_acc));
            }
        }

        // Sign of the key's accidental: +1, −1, or 0 (no accidental / no key).
        let key_dir: i32 = key_acc.map(|a| a.offset.signum()).unwrap_or(0);

        // Collect (score, nat_idx, acc_order, name).
        // acc_order −1 is reserved for plain naturals so they sort before accidentals
        // within the same numeric score.
        let mut candidates: Vec<(i32, usize, i64, String)> = Vec::new();

        for (ni, nat) in self.naturals.iter().enumerate() {
            if nat.degree == degree {
                candidates.push((0, ni, -1, nat.name.clone()));
            }
            for (ai, acc) in self.accidentals.iter().enumerate() {
                let d = (nat.degree as i32 + acc.offset).rem_euclid(edo as i32) as u32;
                if d == degree {
                    let score = if key_dir == 0 || acc.offset.signum() == key_dir {
                        // Neutral or same-direction: prefer smaller accidentals.
                        acc.offset.abs()
                    } else {
                        // Opposite direction: strongly penalise.
                        100 + acc.offset.abs()
                    };
                    candidates.push((score, ni, ai as i64, self.format_note(nat, Some(acc))));
                }
            }
        }

        candidates
            .into_iter()
            .min_by_key(|(score, ni, ai, _)| (*score, *ni, *ai))
            .map(|(_, _, _, name)| name)
    }

    /// Return the canonical name for `degree` with no key bias.
    pub fn name_for_degree(&self, degree: u32, edo: u32) -> Option<String> {
        self.name_for_degree_biased(degree, edo, None, None)
    }

    // ── Bulk generation ───────────────────────────────────────────────────

    /// Generate a flat `Vec<String>` of note names for degrees `0..edo`,
    /// biased toward the given key's accidental direction.
    pub fn note_names_for_key(
        &self,
        edo: u32,
        key_natural_idx: Option<usize>,
        key_accidental_idx: Option<usize>,
    ) -> Vec<String> {
        (0..edo)
            .map(|d| {
                self.name_for_degree_biased(d, edo, key_natural_idx, key_accidental_idx)
                    .unwrap_or_else(|| d.to_string())
            })
            .collect()
    }

    /// Generate a flat `Vec<String>` of note names for degrees `0..edo`
    /// with no key bias (naturals first, then smaller accidentals).
    pub fn note_names(&self, edo: u32) -> Vec<String> {
        self.note_names_for_key(edo, None, None)
    }
}
