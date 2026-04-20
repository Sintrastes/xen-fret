uniffi::setup_scaffolding!();

use app_common::diagrams::build_svg;
use app_common::state::{AppState, DiagramMode, DiagramSettings};
use app_common::storage;
use std::sync::Mutex;
use xen_theory::dataset::{default_scales, default_temperaments, default_tunings};
use xen_theory::instrument::Instrument;

// ── Mirror types ─────────────────────────────────────────────────────────────

#[derive(uniffi::Record)]
pub struct TemperamentRecord {
    pub name: String,
    pub divisions: u32,
    pub period_num: u32,
    pub period_den: u32,
}

#[derive(uniffi::Record)]
pub struct ScaleRecord {
    pub temperament_name: String,
    pub name: String,
    pub intervals: Vec<i32>,
}

#[derive(uniffi::Record)]
pub struct TuningRecord {
    pub temperament_name: String,
    pub name: String,
    pub instrument: String,
    pub string_tunings: Vec<i32>,
    pub skip_frets: u32,
    pub root_octave: i32,
}

#[derive(uniffi::Record)]
pub struct InstrumentRecord {
    pub name: String,
    pub instrument_type: String,
    pub temperament_name: String,
    pub num_strings: u32,
    pub num_frets: u32,
}

#[derive(uniffi::Record)]
pub struct DiagramSettingsRecord {
    pub key: u32,
    pub fret_offset: u32,
    pub num_frets: u32,
    pub display_markers: bool,
    pub horizontal: bool,
    pub dark_mode: bool,
}

// ── Rasterization helpers (mirrors app/gtk_frontend/src/render.rs) ────────────

static BRAVURA_BYTES: &[u8] = include_bytes!("../../dioxus_frontend/assets/Bravura.woff");

fn set_concrete_size(svg: &str, target_w: f32) -> std::borrow::Cow<str> {
    if !svg.contains(r#"width="100%""#) {
        return svg.into();
    }
    if let Some(start) = svg.find(r#"viewBox=""#) {
        let rest = &svg[start + 9..];
        if let Some(end) = rest.find('"') {
            let parts: Vec<f32> = rest[..end]
                .split_ascii_whitespace()
                .filter_map(|s| s.parse().ok())
                .collect();
            if parts.len() == 4 && parts[2] > 0.0 {
                let target_h = target_w * parts[3] / parts[2];
                return svg
                    .replacen(
                        r#"width="100%""#,
                        &format!(r#"width="{target_w}" height="{target_h}""#),
                        1,
                    )
                    .into();
            }
        }
    }
    svg.into()
}

fn rewrite_font_families(svg: &str) -> String {
    svg.replace(
        r#"font-family="Bravura, sans-serif""#,
        r#"font-family="Bravura Text, sans-serif""#,
    )
    .replace(
        r#"font-family='Bravura, sans-serif'"#,
        r#"font-family='Bravura Text, sans-serif'"#,
    )
    .replace(r#"font-family="Bravura""#, r#"font-family="Bravura Text""#)
    .replace(r#"font-family='Bravura'"#, r#"font-family='Bravura Text'"#)
}

fn build_usvg_opts() -> usvg::Options<'static> {
    let mut opts = usvg::Options::default();
    opts.fontdb_mut().load_system_fonts();

    // fontdb skips Android in load_system_fonts(); load manually and fix generic family mappings.
    #[cfg(target_os = "android")]
    {
        for dir in ["/system/fonts", "/system/font", "/data/fonts"] {
            if std::path::Path::new(dir).is_dir() {
                opts.fontdb_mut().load_fonts_dir(dir);
            }
        }
        opts.fontdb_mut().set_sans_serif_family("Roboto");
        opts.fontdb_mut().set_serif_family("Noto Serif");
        opts.fontdb_mut().set_monospace_family("Roboto Mono");
    }

    if let Some(sfnt) = app_common::font::woff1_to_sfnt(BRAVURA_BYTES) {
        opts.fontdb_mut().load_font_data(sfnt);
    }
    opts
}

fn rasterize_to_png(svg: &str, opts: &usvg::Options) -> Option<Vec<u8>> {
    let svg = set_concrete_size(svg, 800.0);
    let svg = rewrite_font_families(&svg);
    let tree = usvg::Tree::from_str(&svg, opts).ok()?;
    let w = tree.size().width().ceil() as u32;
    let h = tree.size().height().ceil() as u32;
    if w == 0 || h == 0 {
        return None;
    }
    let mut pixmap = tiny_skia::Pixmap::new(w, h)?;
    resvg::render(
        &tree,
        tiny_skia::Transform::identity(),
        &mut pixmap.as_mut(),
    );
    pixmap.encode_png().ok()
}

// ── One-time initialization ───────────────────────────────────────────────────

#[uniffi::export]
pub fn xen_fret_init(data_dir: String) {
    storage::set_data_dir(std::path::PathBuf::from(data_dir));
}

// ── Default dataset accessors ─────────────────────────────────────────────────

#[uniffi::export]
pub fn get_default_temperaments() -> Vec<TemperamentRecord> {
    default_temperaments()
        .into_iter()
        .map(|t| TemperamentRecord {
            name: t.name,
            divisions: t.divisions,
            period_num: t.period.numer().clone(),
            period_den: t.period.denom().clone(),
        })
        .collect()
}

#[uniffi::export]
pub fn get_default_scales() -> Vec<ScaleRecord> {
    default_scales()
        .into_iter()
        .map(|s| ScaleRecord {
            temperament_name: s.temperament_name,
            name: s.name,
            intervals: s.intervals,
        })
        .collect()
}

#[uniffi::export]
pub fn get_default_tunings() -> Vec<TuningRecord> {
    default_tunings()
        .into_iter()
        .map(|t| TuningRecord {
            temperament_name: t.temperament.name,
            name: t.name,
            instrument: t.instrument,
            string_tunings: t.string_tunings,
            skip_frets: t.skip_frets,
            root_octave: t.root_octave,
        })
        .collect()
}

// ── XenFretApi — stateful object ──────────────────────────────────────────────

#[derive(uniffi::Object)]
pub struct XenFretApi {
    state: Mutex<AppState>,
    usvg_opts: usvg::Options<'static>,
}

#[uniffi::export]
impl XenFretApi {
    #[uniffi::constructor]
    pub fn new() -> Self {
        Self {
            state: Mutex::new(storage::load()),
            usvg_opts: build_usvg_opts(),
        }
    }

    pub fn get_temperaments(&self) -> Vec<TemperamentRecord> {
        self.state
            .lock()
            .unwrap()
            .temperaments
            .iter()
            .map(|t| TemperamentRecord {
                name: t.name.clone(),
                divisions: t.divisions,
                period_num: t.period.numer().clone(),
                period_den: t.period.denom().clone(),
            })
            .collect()
    }

    pub fn get_instruments(&self) -> Vec<InstrumentRecord> {
        self.state
            .lock()
            .unwrap()
            .instruments
            .iter()
            .map(|i| InstrumentRecord {
                name: i.name.clone(),
                instrument_type: i.instrument_type.clone(),
                temperament_name: i.temperament_name.clone(),
                num_strings: i.num_strings,
                num_frets: i.num_frets,
            })
            .collect()
    }

    pub fn get_scales(&self) -> Vec<ScaleRecord> {
        let state = self.state.lock().unwrap();
        let temp_name = state.current_temperament().map(|t| t.name.clone());
        state
            .scales
            .iter()
            .filter(|s| {
                temp_name
                    .as_deref()
                    .map_or(true, |n| s.temperament_name == n)
            })
            .map(|s| ScaleRecord {
                temperament_name: s.temperament_name.clone(),
                name: s.name.clone(),
                intervals: s.intervals.clone(),
            })
            .collect()
    }

    pub fn get_tunings(&self) -> Vec<TuningRecord> {
        let state = self.state.lock().unwrap();
        let inst = state.current_instrument();
        let temp_name = state.current_temperament().map(|t| t.name.clone());
        let num_strings = inst.map(|i| i.num_strings);
        state
            .tunings
            .iter()
            .filter(|t| {
                temp_name
                    .as_deref()
                    .map_or(true, |n| t.temperament.name == n)
                    && num_strings.map_or(true, |ns| t.string_tunings.len() as u32 == ns)
            })
            .map(|t| TuningRecord {
                temperament_name: t.temperament.name.clone(),
                name: t.name.clone(),
                instrument: t.instrument.clone(),
                string_tunings: t.string_tunings.clone(),
                skip_frets: t.skip_frets,
                root_octave: t.root_octave,
            })
            .collect()
    }

    pub fn save(&self) {
        storage::save(&self.state.lock().unwrap());
    }

    pub fn add_instrument(&self, instrument: InstrumentRecord) {
        self.state.lock().unwrap().add_instrument(Instrument {
            name: instrument.name,
            instrument_type: instrument.instrument_type,
            temperament_name: instrument.temperament_name,
            num_strings: instrument.num_strings,
            num_frets: instrument.num_frets,
            fret_markers: vec![],
            left_handed: None,
        });
    }

    pub fn select_instrument(&self, idx: u32) {
        self.state.lock().unwrap().select_instrument(idx as usize);
    }

    pub fn select_tuning(&self, idx: u32) {
        self.state.lock().unwrap().selected_tuning_idx = idx as usize;
    }

    pub fn select_scale(&self, idx: u32) {
        self.state.lock().unwrap().selected_scale_idx = idx as usize;
    }

    pub fn set_key(&self, key: u32) {
        self.state.lock().unwrap().diagram_settings.key = key;
    }

    pub fn set_fret_offset(&self, offset: u32) {
        self.state
            .lock()
            .unwrap()
            .diagram_settings
            .fretboard_style
            .fret_offset = offset;
    }

    pub fn set_dark_mode(&self, dark: bool) {
        self.state.lock().unwrap().system_dark = dark;
    }

    pub fn set_diagram_mode_scale(&self) {
        self.state.lock().unwrap().diagram_settings.mode = DiagramMode::Scale;
    }

    pub fn set_diagram_mode_chord(&self) {
        self.state.lock().unwrap().diagram_settings.mode = DiagramMode::Chord;
    }

    pub fn generate_diagram_png(&self) -> Option<Vec<u8>> {
        let state = self.state.lock().unwrap().clone();
        let svg = build_svg(&state, "")?;
        rasterize_to_png(&svg, &self.usvg_opts)
    }
}
