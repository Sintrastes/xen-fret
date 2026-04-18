//! Parser and serializer for Scala scale (.scl) files.
//!
//! The Scala format stores one scale per file.  Comment lines begin with `!`
//! and may appear anywhere.  The first non-comment line is a free-text
//! description; the second is the note count; the following *N* lines are
//! pitch values (cents or ratios).  The implicit root of 1/1 (0.0 ¢) is not
//! stored in the file.

use std::fmt;
use std::str::FromStr;

/// A single pitch degree in a Scala scale file.
#[derive(Debug, Clone, PartialEq)]
pub enum Pitch {
    /// A value in cents (the source token contained a `.`).
    Cents(f64),
    /// An exact ratio `numerator/denominator`.
    /// Plain integers (no `.` or `/`) are stored as `Ratio(n, 1)`.
    Ratio(u64, u64),
}

impl Pitch {
    /// Convert to cents (1200 × log₂(ratio) for ratios).
    pub fn to_cents(&self) -> f64 {
        match self {
            Pitch::Cents(c) => *c,
            Pitch::Ratio(n, d) => 1200.0 * (*n as f64 / *d as f64).log2(),
        }
    }
}

impl fmt::Display for Pitch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pitch::Cents(c) => write!(f, "{c:.5}"),
            Pitch::Ratio(n, 1) => write!(f, "{n}"),
            Pitch::Ratio(n, d) => write!(f, "{n}/{d}"),
        }
    }
}

/// A parsed Scala scale file.
#[derive(Debug, Clone, PartialEq)]
pub struct SclFile {
    /// Free-text description (first non-comment line, may be empty).
    pub description: String,
    /// Pitch degrees above the implicit 1/1 root (length equals the note count).
    pub pitches: Vec<Pitch>,
}

// ── Error ──────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum SclError {
    MissingDescription,
    MissingNoteCount,
    InvalidNoteCount(String),
    InvalidPitch(String),
    UnexpectedEof { expected: usize, got: usize },
}

impl fmt::Display for SclError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SclError::MissingDescription => write!(f, "missing description line"),
            SclError::MissingNoteCount => write!(f, "missing note count line"),
            SclError::InvalidNoteCount(s) => write!(f, "invalid note count: {s:?}"),
            SclError::InvalidPitch(s) => write!(f, "invalid pitch: {s:?}"),
            SclError::UnexpectedEof { expected, got } => {
                write!(f, "expected {expected} pitches, found {got}")
            }
        }
    }
}

impl std::error::Error for SclError {}

// ── Parsing ────────────────────────────────────────────────────────────────────

fn parse_pitch(line: &str) -> Result<Pitch, SclError> {
    // The spec says "anything after a valid pitch value should be ignored",
    // so we only look at the first whitespace-delimited token.
    let token = line
        .split_whitespace()
        .next()
        .filter(|t| !t.is_empty())
        .ok_or_else(|| SclError::InvalidPitch(line.to_string()))?;

    if token.contains('.') {
        // Cents — negative values are valid (e.g. -5.0)
        token
            .parse::<f64>()
            .map(Pitch::Cents)
            .map_err(|_| SclError::InvalidPitch(line.to_string()))
    } else if token.contains('/') {
        let (numer_str, denom_str) = token
            .split_once('/')
            .ok_or_else(|| SclError::InvalidPitch(line.to_string()))?;
        let numer: u64 = numer_str
            .parse()
            .map_err(|_| SclError::InvalidPitch(line.to_string()))?;
        let denom: u64 = denom_str
            .parse()
            .map_err(|_| SclError::InvalidPitch(line.to_string()))?;
        if denom == 0 {
            return Err(SclError::InvalidPitch(line.to_string()));
        }
        Ok(Pitch::Ratio(numer, denom))
    } else {
        // Plain integer — treat as N/1
        token
            .parse::<u64>()
            .map(|n| Pitch::Ratio(n, 1))
            .map_err(|_| SclError::InvalidPitch(line.to_string()))
    }
}

impl FromStr for SclFile {
    type Err = SclError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Comments (lines starting with `!`) are stripped first; they may
        // appear anywhere in the file including between pitch values.
        let mut content = s
            .lines()
            .filter(|l| !l.starts_with('!'));

        let description = content
            .next()
            .ok_or(SclError::MissingDescription)?
            .trim()
            .to_string();

        let count_str = content
            .next()
            .ok_or(SclError::MissingNoteCount)?
            .trim()
            .to_string();
        let count: usize = count_str
            .parse()
            .map_err(|_| SclError::InvalidNoteCount(count_str.clone()))?;

        let mut pitches = Vec::with_capacity(count);
        for line in content.take(count) {
            pitches.push(parse_pitch(line.trim())?);
        }

        if pitches.len() < count {
            return Err(SclError::UnexpectedEof {
                expected: count,
                got: pitches.len(),
            });
        }

        Ok(SclFile { description, pitches })
    }
}

// ── Serialization ──────────────────────────────────────────────────────────────

impl fmt::Display for SclFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.description)?;
        writeln!(f, " {}", self.pitches.len())?;
        writeln!(f, "!")?;
        for pitch in &self.pitches {
            writeln!(f, " {pitch}")?;
        }
        Ok(())
    }
}

// ── Tests ──────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    const MEANQUAR: &str = "\
! meanquar.scl
!
1/4-comma meantone scale. Pietro Aaron's temperament (1523)
 12
!
 76.04900
 193.15686
 310.26471
 5/4
 503.42157
 579.47057
 696.57843
 25/16
 889.73529
 1006.84314
 1082.89214
 2/1
";

    #[test]
    fn parse_meanquar() {
        let scl: SclFile = MEANQUAR.parse().unwrap();
        assert_eq!(scl.description, "1/4-comma meantone scale. Pietro Aaron's temperament (1523)");
        assert_eq!(scl.pitches.len(), 12);
        assert_eq!(scl.pitches[3], Pitch::Ratio(5, 4));
        assert_eq!(scl.pitches[7], Pitch::Ratio(25, 16));
        assert_eq!(scl.pitches[11], Pitch::Ratio(2, 1));
        assert!(matches!(scl.pitches[0], Pitch::Cents(c) if (c - 76.04900).abs() < 1e-5));
    }

    #[test]
    fn parse_integer_pitch() {
        let scl: SclFile = "test\n 1\n 2\n".parse().unwrap();
        assert_eq!(scl.pitches[0], Pitch::Ratio(2, 1));
    }

    #[test]
    fn parse_ignores_trailing_annotation() {
        // "100.0 cents" and "5/4   E\\" are both valid per the spec
        let scl: SclFile = "test\n 2\n 100.0 cents\n 5/4   E\\\n".parse().unwrap();
        assert_eq!(scl.pitches[0], Pitch::Cents(100.0));
        assert_eq!(scl.pitches[1], Pitch::Ratio(5, 4));
    }

    #[test]
    fn parse_negative_cents() {
        let scl: SclFile = "test\n 1\n -5.0\n".parse().unwrap();
        assert!(matches!(scl.pitches[0], Pitch::Cents(c) if (c + 5.0).abs() < 1e-10));
    }

    #[test]
    fn roundtrip() {
        let original: SclFile = MEANQUAR.parse().unwrap();
        let serialized = original.to_string();
        let reparsed: SclFile = serialized.parse().unwrap();
        assert_eq!(original.description, reparsed.description);
        assert_eq!(original.pitches, reparsed.pitches);
    }

    #[test]
    fn error_on_missing_description() {
        assert_eq!("".parse::<SclFile>().unwrap_err(), SclError::MissingDescription);
    }

    #[test]
    fn error_on_too_few_pitches() {
        let err = "test\n 3\n 100.0\n".parse::<SclFile>().unwrap_err();
        assert!(matches!(err, SclError::UnexpectedEof { expected: 3, got: 1 }));
    }
}
