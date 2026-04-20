use num::rational::Ratio;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Temperament {
    pub name: String,
    /// Number of equal divisions of the period (e.g. 12 for 12-TET)
    pub divisions: u32,
    /// Period as a ratio (numerator, denominator), e.g. 2 / 1 for an octave
    pub period: Ratio<u32>,
}

impl Temperament {
    pub fn with_name(mut self, name: &str) -> Temperament {
        self.name = name.to_string();
        self
    }

    pub fn period_f64(&self) -> f64 {
        *self.period.numer() as f64 / *self.period.denom() as f64
    }

    pub fn period_str(&self) -> String {
        if *self.period.denom() == 1 {
            format!("{}/1", self.period.numer())
        } else {
            format!("{}/{}", self.period.numer(), self.period.denom())
        }
    }

    ////////////////// Builtin temperaments ///////////////////

    pub fn bohlen_pierce() -> Temperament {
        edt(13).with_name("Bohlen Pierce")
    }
}

pub fn edo(divisions: u32) -> Temperament {
    Temperament {
        // TODO: Change to -EDO after dataset is migrated.
        name: format!("{}-TET", divisions).into(),
        divisions: divisions,
        period: Ratio::new(2, 1),
    }
}

pub fn edt(divisions: u32) -> Temperament {
    Temperament {
        name: format!("{}-EDT", divisions).into(),
        divisions: divisions,
        period: Ratio::new(3, 1),
    }
}
