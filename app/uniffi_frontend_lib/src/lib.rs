uniffi::setup_scaffolding!();

use app_common::diagrams::build_svg_with_layout;
use app_common::hit_test::hit_test;
use app_common::state::{AppState, DiagramMode};
use app_common::storage;
use fretboard_diagrams::DiagramLayout;
use std::sync::Mutex;
use xen_theory::dataset::{default_scales, default_temperaments, default_tunings};
use xen_theory::instrument::Instrument;

#[derive(uniffi::Record)]
pub struct HitTargetRecord {
    pub string_idx: u32,
    pub fret: i32,
    pub scale_degree: u32,
    pub absolute_step: i32,
}

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

static SF2_BYTES: &[u8] = include_bytes!(
    "../../dioxus_frontend/assets/soundfont.sf2"
);

#[derive(uniffi::Object)]
pub struct XenFretApi {
    state: Mutex<AppState>,
    usvg_opts: usvg::Options<'static>,
    layout: Mutex<Option<DiagramLayout>>,
    flash_steps: Mutex<Vec<i32>>,
    audio: Mutex<Option<xen_sequencer::native_audio::NativeAudioHandle>>,
    mic: Mutex<Option<std::sync::Arc<xen_sequencer::native_mic::NativeMicHandle>>>,
    mic_steps: std::sync::Arc<Mutex<Vec<i32>>>,
}

#[uniffi::export]
impl XenFretApi {
    #[uniffi::constructor]
    pub fn new() -> Self {
        let sf = xen_sequencer::native_audio::soundfont_from_bytes(SF2_BYTES);
        let audio = xen_sequencer::native_audio::NativeAudioHandle::spawn(sf);
        Self {
            state: Mutex::new(storage::load()),
            usvg_opts: build_usvg_opts(),
            layout: Mutex::new(None),
            flash_steps: Mutex::new(Vec::new()),
            audio: Mutex::new(audio),
            mic: Mutex::new(None),
            mic_steps: std::sync::Arc::new(Mutex::new(Vec::new())),
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
        let mut flash = self.flash_steps.lock().unwrap().clone();
        flash.extend(self.mic_steps.lock().unwrap().iter().copied());
        let (svg, diagram_layout) = build_svg_with_layout(&state, "", &[], &flash)?;
        *self.layout.lock().unwrap() = Some(diagram_layout);
        rasterize_to_png(&svg, &self.usvg_opts)
    }

    pub fn hit_test_diagram(
        &self,
        elem_w: f64,
        elem_h: f64,
        x: f64,
        y: f64,
    ) -> Option<HitTargetRecord> {
        let layout = self.layout.lock().unwrap();
        let layout = layout.as_ref()?;
        hit_test(layout, elem_w, elem_h, x, y).map(|h| HitTargetRecord {
            string_idx: h.string_idx as u32,
            fret: h.fret,
            scale_degree: h.scale_degree as u32,
            absolute_step: h.absolute_step,
        })
    }

    pub fn push_flash_step(&self, step: i32) {
        self.flash_steps.lock().unwrap().push(step);
    }

    pub fn pop_flash_step(&self, step: i32) {
        let mut v = self.flash_steps.lock().unwrap();
        if let Some(pos) = v.iter().position(|s| *s == step) {
            v.remove(pos);
        }
    }

    /// Start microphone pitch detection. Returns `true` if started successfully.
    /// Detected steps are automatically included in subsequent `generate_diagram_png` calls.
    pub fn start_mic(&self) -> bool {
        let mut g = self.mic.lock().unwrap();
        if g.is_some() { return true; }
        let Some(h) = xen_sequencer::native_mic::NativeMicHandle::spawn(4096) else { return false; };
        let handle = std::sync::Arc::new(h);
        *g = Some(handle.clone());
        drop(g);

        let state_snapshot = self.state.lock().unwrap().clone();
        let detector_kind = state_snapshot.preferences.pitch_detector.clone();
        let mic_steps = self.mic_steps.clone();
        std::thread::Builder::new()
            .name("xen-mic-detect".into())
            .spawn(move || {
                use xen_dsp::detector::PitchDetector;
                use app_common::preferences::PitchDetectorKind;
                let mut detector: Box<dyn PitchDetector> = match detector_kind {
                    PitchDetectorKind::Yin => {
                        Box::new(xen_dsp::yin::YinDetector::new(0.15, 50.0, 4000.0))
                    }
                    PitchDetectorKind::IterF0 => Box::new(
                        xen_dsp::iterf0::IterF0Detector::new(6, 8, 70.0, 2000.0, 0.15),
                    ),
                };
                let mut holdoff: std::collections::HashMap<i32, u8> = Default::default();
                const HOLDOFF_FRAMES: u8 = 12;
                while let Some(ev) = handle.next_event_blocking() {
                    match ev {
                        xen_sequencer::mic::MicEvent::Samples { rate, data } => {
                            let pitches = detector.detect(&data, rate);
                            let fresh = app_common::pitch_map::detected_to_absolute_steps(
                                &state_snapshot,
                                &pitches,
                            );
                            holdoff.retain(|_, c| {
                                if *c == 0 { false } else { *c -= 1; true }
                            });
                            for s in fresh {
                                holdoff.insert(s, HOLDOFF_FRAMES);
                            }
                            *mic_steps.lock().unwrap() = holdoff.keys().copied().collect();
                        }
                        xen_sequencer::mic::MicEvent::Stopped
                        | xen_sequencer::mic::MicEvent::Error(_) => {
                            mic_steps.lock().unwrap().clear();
                            break;
                        }
                        xen_sequencer::mic::MicEvent::Started { .. } => {}
                    }
                }
            })
            .is_ok()
    }

    /// Stop microphone pitch detection and clear detected steps.
    pub fn stop_mic(&self) {
        if let Some(h) = self.mic.lock().unwrap().take() {
            h.stop();
        }
        self.mic_steps.lock().unwrap().clear();
    }

    /// Play the pitch corresponding to `absolute_step` in the current temperament/tuning.
    pub fn play_step(&self, absolute_step: i32) {
        let state = self.state.lock().unwrap();
        let Some(temp) = state.current_temperament().cloned() else { return };
        let Some(tuning) = state.current_tuning().cloned() else { return };
        let prefs = state.preferences.clone();
        drop(state);

        let root_hz = tuning.step0_hz(
            prefs.concert_hz,
            prefs.concert_octave,
            temp.divisions,
            (*temp.period.numer(), *temp.period.denom()),
        );
        let freq = xen_theory::theory::edo_step_to_hz(
            absolute_step,
            temp.divisions,
            (*temp.period.numer(), *temp.period.denom()),
            root_hz,
        );

        let mut guard = self.audio.lock().unwrap();
        if guard.is_none() {
            let sf = xen_sequencer::native_audio::soundfont_from_bytes(SF2_BYTES);
            *guard = xen_sequencer::native_audio::NativeAudioHandle::spawn(sf);
        }
        if let Some(handle) = guard.as_ref() {
            handle.play_chord(vec![freq]);
        }
    }
}
