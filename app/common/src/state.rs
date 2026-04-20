use crate::preferences::{Preferences, ThemeMode};
use fretboard_diagrams::{DiagramColors, FretboardStyle};
use num::rational::Ratio;
use xen_theory::chord::Chord;
use xen_theory::dataset::{
    default_chords, default_notation_systems, default_scales, default_temperaments, default_tunings,
};
use xen_theory::instrument::Instrument;
use xen_theory::notation::NotationSystem;
use xen_theory::scale::Scale;
use xen_theory::temperament::Temperament;
use xen_theory::tuning::Tuning;

#[derive(Debug, Clone, PartialEq)]
pub struct DiagramSettings {
    pub key: u32,
    pub key_natural_idx: Option<usize>,
    pub key_accidental_idx: Option<usize>, // None = no accidental
    pub fret_offset: u32,
    pub num_frets: u32,
    pub num_frets_h: u32,
    pub display_size: u32,
    pub fretboard_style: FretboardStyle,
    pub mode: DiagramMode,
    /// Octave offset for playback relative to the tuning's root_octave. Default: 0.
    pub playback_octave: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DiagramMode {
    Scale,
    Chord,
}

impl DiagramSettings {
    fn default(colors: DiagramColors) -> Self {
        Self {
            key: 0,
            key_natural_idx: None,
            key_accidental_idx: None,
            fret_offset: 0,
            num_frets: 10,
            num_frets_h: 36,
            display_size: 350,
            fretboard_style: FretboardStyle::default(colors),
            mode: DiagramMode::Scale,
            playback_octave: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AppState {
    pub temperaments: Vec<Temperament>,
    pub notation_systems: Vec<NotationSystem>,
    pub tunings: Vec<Tuning>,
    pub scales: Vec<Scale>,
    pub chords: Vec<Chord>,
    pub instruments: Vec<Instrument>,
    pub preferences: Preferences,

    pub selected_instrument_idx: Option<usize>,
    pub selected_tuning_idx: usize,
    pub selected_scale_idx: usize,
    pub selected_chord_idx: usize,
    pub diagram_settings: DiagramSettings,
    /// Runtime-only: current system dark/light preference (not persisted).
    pub system_dark: bool,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            temperaments: default_temperaments(),
            notation_systems: default_notation_systems(),
            tunings: default_tunings(),
            scales: default_scales(),
            chords: default_chords(),
            instruments: vec![],
            preferences: Preferences::default(),
            diagram_settings: DiagramSettings::default(DiagramColors::default_light()),
            selected_instrument_idx: None,
            selected_tuning_idx: 0,
            selected_scale_idx: 0,
            selected_chord_idx: 0,
            system_dark: false,
        }
    }
}

impl AppState {
    /// Returns the effective dark mode based on user preference and detected system theme.
    pub fn effective_dark_mode(&self) -> bool {
        match self.preferences.theme_mode {
            ThemeMode::Dark => true,
            ThemeMode::Light => false,
            ThemeMode::System => self.system_dark,
        }
    }

    /// After loading from storage, restore `selected_instrument_idx` from `preferences.default_instrument`.
    pub fn restore_selections(&mut self) {
        if let Some(ref name) = self.preferences.default_instrument.clone() {
            self.selected_instrument_idx = self.instruments.iter().position(|i| &i.name == name);
        }
    }

    pub fn current_instrument(&self) -> Option<&Instrument> {
        self.selected_instrument_idx
            .and_then(|i| self.instruments.get(i))
    }

    pub fn current_temperament(&self) -> Option<&Temperament> {
        self.current_instrument().and_then(|inst| {
            self.temperaments
                .iter()
                .find(|t| t.name == inst.temperament_name)
        })
    }

    pub fn current_notation_system(&self) -> Option<&NotationSystem> {
        let temp = self.current_temperament()?;
        // Prefer the user's sticky choice for this temperament.
        if let Some(ns_name) = self.preferences.notation_system_prefs.get(&temp.name) {
            if let Some(ns) = self
                .notation_systems
                .iter()
                .find(|ns| ns.temperament_name == temp.name && &ns.name == ns_name)
            {
                return Some(ns);
            }
        }
        self.notation_systems
            .iter()
            .find(|ns| ns.temperament_name == temp.name)
    }

    /// Returns tunings from the current temperament that match the current instrument's string count.
    pub fn compatible_tunings(&self) -> Vec<(usize, &Tuning)> {
        let Some(inst) = self.current_instrument() else {
            return vec![];
        };
        let Some(temp) = self.current_temperament() else {
            return vec![];
        };
        self.tunings
            .iter()
            .enumerate()
            .filter(|(_, tu)| {
                tu.temperament.name == temp.name
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
        self.scales
            .iter()
            .filter(|s| s.temperament_name == temp.name)
            .nth(self.selected_scale_idx)
    }

    pub fn current_chord(&self) -> Option<&Chord> {
        let temp = self.current_temperament()?;
        self.chords
            .iter()
            .filter(|c| c.temperament_name == temp.name)
            .nth(self.selected_chord_idx)
    }

    // ── Scale CRUD ────────────────────────────────────────────────────────────

    pub fn add_scale(&mut self, scale: Scale) {
        self.scales.push(scale);
    }

    pub fn update_scale(&mut self, idx: usize, name: String, intervals: Vec<i32>) {
        if let Some(sc) = self.scales.get_mut(idx) {
            sc.name = name;
            sc.intervals = intervals;
        }
    }

    pub fn delete_scale(&mut self, idx: usize) {
        if idx < self.scales.len() {
            self.scales.remove(idx);
            self.selected_scale_idx = 0;
        }
    }

    // ── Chord CRUD ────────────────────────────────────────────────────────────

    pub fn add_chord(&mut self, chord: Chord) {
        self.chords.push(chord);
    }

    pub fn update_chord(&mut self, idx: usize, name: String, intervals: Vec<i32>) {
        if let Some(ch) = self.chords.get_mut(idx) {
            ch.name = name;
            ch.intervals = intervals;
        }
    }

    pub fn delete_chord(&mut self, idx: usize) {
        if idx < self.chords.len() {
            self.chords.remove(idx);
            self.selected_chord_idx = 0;
        }
    }

    // ── Tuning CRUD ───────────────────────────────────────────────────────────

    pub fn add_tuning(&mut self, tuning: Tuning) {
        self.tunings.push(tuning);
    }

    pub fn update_tuning(
        &mut self,
        idx: usize,
        name: String,
        instrument: String,
        string_tunings: Vec<i32>,
        skip_frets: u32,
        root_octave: i32,
    ) {
        if let Some(tu) = self.tunings.get_mut(idx) {
            tu.name = name;
            tu.instrument = instrument;
            tu.string_tunings = string_tunings;
            tu.skip_frets = skip_frets;
            tu.root_octave = root_octave;
        }
    }

    pub fn delete_tuning(&mut self, idx: usize) {
        if idx < self.tunings.len() {
            self.tunings.remove(idx);
            self.selected_tuning_idx = 0;
        }
    }

    // ── Temperament CRUD ──────────────────────────────────────────────────────

    pub fn add_temperament(&mut self, temperament: Temperament) {
        self.temperaments.push(temperament);
    }

    pub fn update_temperament(
        &mut self,
        idx: usize,
        name: String,
        divisions: u32,
        period: Ratio<u32>,
    ) {
        if let Some(t) = self.temperaments.get_mut(idx) {
            t.name = name;
            t.divisions = divisions;
            t.period = period;
        }
    }

    /// Delete a temperament and cascade-remove all dependent data.
    pub fn delete_temperament(&mut self, idx: usize) {
        if idx >= self.temperaments.len() {
            return;
        }
        let name = self.temperaments[idx].name.clone();
        self.temperaments.remove(idx);
        self.notation_systems
            .retain(|ns| ns.temperament_name != name);
        self.tunings.retain(|tu| tu.temperament.name != name);
        self.scales.retain(|sc| sc.temperament_name != name);
        self.chords.retain(|ch| ch.temperament_name != name);
        if self
            .instruments
            .get(self.selected_instrument_idx.unwrap_or(usize::MAX))
            .map(|i| i.temperament_name.as_str())
            == Some(name.as_str())
        {
            self.selected_instrument_idx = None;
        }
    }

    // ── Instrument CRUD ───────────────────────────────────────────────────────

    pub fn add_instrument(&mut self, instrument: Instrument) {
        self.instruments.push(instrument);
    }

    pub fn update_instrument(&mut self, idx: usize, instrument: Instrument) {
        if let Some(slot) = self.instruments.get_mut(idx) {
            *slot = instrument;
        }
    }

    pub fn delete_instrument(&mut self, idx: usize) {
        if idx < self.instruments.len() {
            self.instruments.remove(idx);
            self.selected_instrument_idx = match self.selected_instrument_idx {
                Some(sel) if sel == idx => None,
                Some(sel) if sel > idx => Some(sel - 1),
                other => other,
            };
        }
    }

    /// Select an instrument by index and reset dependent selections.
    pub fn select_instrument(&mut self, idx: usize) {
        self.selected_instrument_idx = Some(idx);
        if let Some(name) = self.instruments.get(idx).map(|i| i.name.clone()) {
            self.preferences.default_instrument = Some(name);
        }
        self.selected_tuning_idx = 0;
        self.selected_scale_idx = 0;
        self.selected_chord_idx = 0;
        self.diagram_settings.key = 0;
        self.diagram_settings.key_natural_idx = None;
        self.diagram_settings.key_accidental_idx = None;
    }
}
