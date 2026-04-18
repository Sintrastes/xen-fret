#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Scale {
    #[cfg_attr(feature = "serde", serde(default))]
    pub temperament_name: String,
    pub name: String,
    /// Intervals between notes (in EDO steps)
    pub intervals: Vec<i32>,
}
