///
/// A chord is a set of intervals in a given temperament typically
///  played together.
///
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Chord {
    #[cfg_attr(feature = "serde", serde(default))]
    pub temperament_name: String,
    pub name: String,
    pub intervals: Vec<i32>,
}
