#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Temperament {
    pub name: String,
    /// Number of equal divisions of the period (e.g. 12 for 12-TET)
    pub divisions: u32,
    /// Period as a ratio (numerator, denominator), e.g. (2, 1) for an octave
    pub period: (u32, u32),
}

impl Temperament {
    pub fn period_str(&self) -> String {
        if self.period.1 == 1 {
            format!("{}/1", self.period.0)
        } else {
            format!("{}/{}", self.period.0, self.period.1)
        }
    }
}
