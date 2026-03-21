use crate::models::{
    default_chords, default_notation_systems, default_scales, default_temperaments,
    default_tunings, Chord, Instrument, Preferences, Scale, Temperament, Tuning,
};
use crate::notation::NotationSystem;
use dioxus::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub struct DiagramSettings {
    pub key: u32,
    pub key_natural_idx: Option<usize>,
    pub key_accidental_idx: Option<usize>,  // None = no accidental
    pub fret_offset: u32,
    pub num_frets: u32,
    pub num_frets_h: u32,
    pub display_size: u32,
    pub vertical_spacing: u32,
    pub horizontal_spacing: u32,
    pub display_markers: bool,
    pub horizontal: bool,
    pub mode: DiagramMode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DiagramMode {
    Scale,
    Chord,
}

impl Default for DiagramSettings {
    fn default() -> Self {
        Self {
            key: 0,
            key_natural_idx: None,
            key_accidental_idx: None,
            fret_offset: 0,
            num_frets: 10,
            num_frets_h: 36,
            display_size: 350,
            vertical_spacing: 200,
            horizontal_spacing: 230,
            display_markers: false,
            horizontal: false,
            mode: DiagramMode::Scale,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AppState {
    pub temperaments:     Vec<Temperament>,
    pub notation_systems: Vec<NotationSystem>,
    pub tunings:          Vec<Tuning>,
    pub scales:           Vec<Scale>,
    pub chords:           Vec<Chord>,
    pub instruments:      Vec<Instrument>,
    pub preferences:      Preferences,

    pub selected_instrument_idx: Option<usize>,
    pub selected_tuning_idx: usize,
    pub selected_scale_idx: usize,
    pub selected_chord_idx: usize,
    pub diagram_settings: DiagramSettings,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            temperaments:     default_temperaments(),
            notation_systems: default_notation_systems(),
            tunings:          default_tunings(),
            scales:           default_scales(),
            chords:           default_chords(),
            instruments:      vec![],
            preferences:      Preferences::default(),
            selected_instrument_idx: None,
            selected_tuning_idx: 0,
            selected_scale_idx: 0,
            selected_chord_idx: 0,
            diagram_settings: DiagramSettings::default(),
        }
    }
}

impl AppState {
    /// After loading from storage, restore `selected_instrument_idx` from `preferences.default_instrument`.
    pub fn restore_selections(&mut self) {
        if let Some(ref name) = self.preferences.default_instrument.clone() {
            self.selected_instrument_idx = self.instruments.iter().position(|i| &i.name == name);
        }
    }

    pub fn current_instrument(&self) -> Option<&Instrument> {
        self.selected_instrument_idx.and_then(|i| self.instruments.get(i))
    }

    pub fn current_temperament(&self) -> Option<&Temperament> {
        self.current_instrument()
            .and_then(|inst| self.temperaments.iter().find(|t| t.name == inst.temperament_name))
    }

    pub fn current_notation_system(&self) -> Option<&NotationSystem> {
        let temp = self.current_temperament()?;
        // Prefer the user's sticky choice for this temperament.
        if let Some(ns_name) = self.preferences.notation_system_prefs.get(&temp.name) {
            if let Some(ns) = self.notation_systems.iter()
                .find(|ns| ns.temperament_name == temp.name && &ns.name == ns_name)
            {
                return Some(ns);
            }
        }
        self.notation_systems.iter().find(|ns| ns.temperament_name == temp.name)
    }

    /// Returns tunings from the current temperament that match the current instrument's string count.
    pub fn compatible_tunings(&self) -> Vec<(usize, &Tuning)> {
        let Some(inst) = self.current_instrument() else { return vec![] };
        let Some(temp) = self.current_temperament() else { return vec![] };
        self.tunings.iter().enumerate()
            .filter(|(_, tu)| {
                tu.temperament_name == temp.name
                    && tu.string_tunings.len() == inst.num_strings as usize
            })
            .collect()
    }

    pub fn current_tuning(&self) -> Option<&Tuning> {
        let compatible = self.compatible_tunings();
        compatible.get(self.selected_tuning_idx).map(|(_, tu)| *tu)
    }

    pub fn current_scale(&self) -> Option<&Scale> {
        let temp = self.current_temperament()?;
        self.scales.iter()
            .filter(|s| s.temperament_name == temp.name)
            .nth(self.selected_scale_idx)
    }

    pub fn current_chord(&self) -> Option<&Chord> {
        let temp = self.current_temperament()?;
        self.chords.iter()
            .filter(|c| c.temperament_name == temp.name)
            .nth(self.selected_chord_idx)
    }
}

// GlobalSignal: accessible from any component without context setup.
// Reading it inside a component subscribes that component to re-render on changes.
pub static APP_STATE: GlobalSignal<AppState> = Signal::global(|| crate::storage::load());
