use crate::components::{Combobox, FretboardPreview, Select};
use crate::components::InstrumentPicker;
use crate::fretboard::{render_board, FretboardStyle};
use crate::models::{NotationSystem, Scale};
use crate::state::{DiagramMode, APP_STATE};
use crate::{audio, pitch_tracking, theory};
use dioxus::prelude::*;
use dioxus::document::eval;

static BRAVURA_FONT: Asset = asset!("/assets/Bravura.woff2");

/// Generate the current diagram SVG from app state, or None if no scale/tuning selected.
fn build_svg() -> Option<String> {
    let state = APP_STATE.read();
    let prefs = state.preferences.clone();
    let dark = state.effective_dark_mode();
    let settings = state.diagram_settings.clone();
    let maybe_scale = state.current_scale().cloned();
    let maybe_tuning = state.current_tuning().cloned();
    let edo = state.current_temperament().map(|t| t.divisions).unwrap_or(12);
    let note_names: Vec<String> = state
        .current_notation_system()
        .map(|ns| ns.note_names_for_key(edo, settings.key_natural_idx, settings.key_accidental_idx))
        .unwrap_or_default();
    let period = state.current_temperament()
        .map(|t| t.period.0 as f64 / t.period.1 as f64)
        .unwrap_or(2.0);
    drop(state);

    let (scale, tuning) = match (maybe_scale, maybe_tuning) {
        (Some(s), Some(t)) => (s, t),
        _ => return None,
    };
    let n_frets_h = {
        let n = (edo as f64 * 4_f64.ln() / period.ln()).round() as u32;
        n.clamp(8, 72)
    };
    let default_vs = settings.vertical_spacing as f64 / 1000.0;
    let vs_h = default_vs * 24.0 / n_frets_h as f64;

    let fret_style = FretboardStyle {
        display_markers_on_frets: settings.display_markers,
        fret_offset: settings.fret_offset,
        num_frets: if settings.horizontal { n_frets_h } else { settings.num_frets },
        vertical_spacing: if settings.horizontal { vs_h } else { default_vs },
        horizontal_spacing: settings.horizontal_spacing as f64 / 1000.0,
        horizontal: settings.horizontal,
        edo,
        period,
    };
    let note_names_ref: Option<&[String]> = if note_names.is_empty() { None } else { Some(&note_names) };
    Some(render_board(
        &prefs, &scale.name, settings.key as i32, &scale,
        tuning.skip_frets, &tuning, &fret_style, note_names_ref,
        &BRAVURA_FONT.to_string(), &[], &[], dark,
    ))
}

#[component]
pub fn Home() -> Element {
    let mut drawer_open = use_signal(|| false);
    let mut is_mobile = use_signal(|| false);
    let mut is_landscape = use_signal(|| false);
    let mut show_instrument_picker = use_signal(|| false);
    let mut is_playing = use_signal(|| false);
    let mut playing_degrees: Signal<Vec<usize>> = use_signal(Vec::new);
    let mut mic_active = use_signal(|| false);
    let mut mic_degrees: Signal<Vec<usize>> = use_signal(Vec::new);
    let mut mic_steps: Signal<Vec<i32>> = use_signal(Vec::new);

    let export_svg = move |_| {
        let Some(svg) = build_svg() else { return };
        let filename = {
            let s = APP_STATE.read();
            s.current_scale()
                .map(|sc| format!("{}.svg", sc.name.to_lowercase().replace(' ', "_")))
                .unwrap_or_else(|| "fretboard.svg".to_string())
        };
        let escaped = svg.replace('\\', "\\\\").replace('`', "\\`").replace("${", "\\${");
        let js = format!(
            "const b=new Blob([`{escaped}`],{{type:'image/svg+xml'}});\
             const u=URL.createObjectURL(b);\
             const a=document.createElement('a');\
             a.href=u;a.download={filename:?};a.click();\
             URL.revokeObjectURL(u);"
        );
        let _ = eval(&js);
    };

    let state = APP_STATE.read();
    let has_instrument = state.selected_instrument_idx.is_some();
    let instrument_label = state.current_instrument()
        .map(|i| format!("{} ({})", i.name, i.temperament_name))
        .unwrap_or_default();
    let instrument_type_label = state.current_instrument()
        .map(|i| i.instrument_type.clone())
        .unwrap_or_default();

    let sel_tuning_idx = state.selected_tuning_idx;
    let sel_scale_idx = state.selected_scale_idx;
    let sel_chord_idx = state.selected_chord_idx;
    let settings = state.diagram_settings.clone();
    let is_scale_mode = matches!(settings.mode, DiagramMode::Scale);
    let is_horizontal = settings.horizontal;
    let effective_horizontal = if *is_mobile.read() { *is_landscape.read() } else { is_horizontal };

    let maybe_item: Option<Scale> = if is_scale_mode {
        state.current_scale().cloned()
    } else {
        state.current_chord().map(|c| Scale {
            temperament_name: String::new(),
            name: c.name.clone(),
            intervals: c.intervals.clone(),
        })
    };
    let play_freqs: Vec<f64> = state
        .current_temperament()
        .zip(maybe_item.as_ref())
        .map(|(temp, item)| {
            theory::scale_to_hz_sequence(item, settings.key as i32, temp.divisions, temp.period, 440.0)
        })
        .unwrap_or_default();

    let tuning_options: Vec<(usize, String)> = state.compatible_tunings()
        .iter()
        .enumerate()
        .map(|(i, (_, tu))| (i, tu.name.clone()))
        .collect();

    let scale_options: Vec<(usize, String)> = state
        .current_temperament()
        .map(|t| {
            state.scales.iter()
                .filter(|s| s.temperament_name == t.name)
                .enumerate()
                .map(|(i, s)| (i, s.name.clone()))
                .collect()
        })
        .unwrap_or_default();

    let chord_options: Vec<(usize, String)> = state
        .current_temperament()
        .map(|t| {
            state.chords.iter()
                .filter(|c| c.temperament_name == t.name)
                .enumerate()
                .map(|(i, c)| (i, c.name.clone()))
                .collect()
        })
        .unwrap_or_default();

    let divisions = state
        .current_temperament()
        .map(|t| t.divisions)
        .unwrap_or(12);
    let notation_system: Option<NotationSystem> = state
        .current_notation_system()
        .cloned();
    let notation_options: Vec<(String, String)> = state
        .current_temperament()
        .map(|t| {
            state.notation_systems.iter()
                .filter(|ns| ns.temperament_name == t.name)
                .map(|ns| (ns.name.clone(), if ns.name.is_empty() { "Default".to_string() } else { ns.name.clone() }))
                .collect()
        })
        .unwrap_or_default();
    let current_ns_name = notation_system.as_ref().map(|ns| ns.name.clone()).unwrap_or_default();
    let current_temp_name = state.current_temperament().map(|t| t.name.clone()).unwrap_or_default();
    let effective_nat_idx: Option<usize> = settings.key_natural_idx
        .or_else(|| notation_system.as_ref()
            .and_then(|ns| ns.naturals.iter().position(|n| n.degree == settings.key)))
        .or(Some(0));
    let concert_hz = state.preferences.concert_hz;
    let concert_octave = state.preferences.concert_octave;
    let concert_ref_name = notation_system.as_ref()
        .and_then(|ns| ns.naturals.iter().find(|n| n.degree == 0))
        .map(|n| n.name.clone())
        .unwrap_or_else(|| "Step 0".to_string());
    let concert_label = format!("{}{}", concert_ref_name, concert_octave);
    drop(state);

    let has_freqs = !play_freqs.is_empty();
    let n_degrees = play_freqs.len().saturating_sub(1).max(1);
    let on_play = {
        let freqs = play_freqs;
        move |_| {
            if is_playing() || freqs.is_empty() { return; }
            let f = freqs.clone();
            spawn(async move {
                is_playing.set(true);
                if is_scale_mode {
                    let mut pb = audio::start_scale(&f);
                    while let Some(note_idx) = pb.next_note().await {
                        playing_degrees.set(vec![note_idx % n_degrees]);
                    }
                    playing_degrees.set(vec![]);
                } else {
                    audio::play_chord(&f).await;
                }
                is_playing.set(false);
            });
        }
    };

    let on_mic_toggle = move |_| {
        if mic_active() {
            let _ = eval(pitch_tracking::mic_web::MIC_STOP_JS);
            mic_active.set(false);
            mic_degrees.set(vec![]);
            mic_steps.set(vec![]);
        } else {
            mic_active.set(true);
            let detector_kind = APP_STATE.read().preferences.pitch_detector.clone();
            spawn(async move {
                let mut stream = pitch_tracking::MicStream::start(4096);
                let mut detector: Box<dyn pitch_tracking::PitchDetector> = match detector_kind {
                    crate::models::PitchDetectorKind::Yin =>
                        Box::new(pitch_tracking::YinDetector::new(0.15, 50.0, 4000.0)),
                    crate::models::PitchDetectorKind::IterF0 =>
                        Box::new(pitch_tracking::IterF0Detector::new(6, 8, 70.0, 2000.0, 0.15)),
                };
                let mut holdoff_degs: std::collections::HashMap<usize, u8> =
                    std::collections::HashMap::new();
                let mut holdoff_steps: std::collections::HashMap<i32, u8> =
                    std::collections::HashMap::new();
                const HOLDOFF_FRAMES: u8 = 12;

                while let Some(event) = stream.next_event().await {
                    if !mic_active() { break; }
                    match event {
                        pitch_tracking::MicEvent::Samples { rate, data } => {
                            let pitches = detector.detect(&data, rate);
                            let state = APP_STATE.read();
                            let concert_hz = state.preferences.concert_hz;

                            let (fresh_degs, fresh_steps): (Vec<usize>, Vec<i32>) =
                                if let (Some(temp), Some(scale)) =
                                    (state.current_temperament(), state.current_scale())
                                {
                                    let key = state.diagram_settings.key as i32;
                                    if let Some(lowest_hz) =
                                        state.current_tuning().and_then(|t| t.lowest_string_hz)
                                    {
                                        let string0 = state.current_tuning()
                                            .and_then(|t| t.string_tunings.first().copied())
                                            .unwrap_or(0);
                                        let period_ratio =
                                            temp.period.0 as f64 / temp.period.1 as f64;
                                        let lowest_step = (temp.divisions as f64
                                            * (lowest_hz / 440.0_f64).ln()
                                            / period_ratio.ln())
                                            .round() as i32;
                                        let offset = string0 - lowest_step;
                                        let steps = pitches
                                            .iter()
                                            .filter(|p| p.confidence > 0.1)
                                            .filter_map(|p| {
                                                pitch_tracking::hz_to_absolute_step(
                                                    p.hz,
                                                    temp.divisions,
                                                    temp.period,
                                                    concert_hz,
                                                    key,
                                                    &scale.intervals,
                                                    50.0,
                                                )
                                                .map(|s| s + offset)
                                            })
                                            .collect();
                                        (vec![], steps)
                                    } else {
                                        let degs = pitches
                                            .iter()
                                            .filter(|p| p.confidence > 0.1)
                                            .filter_map(|p| {
                                                pitch_tracking::hz_to_scale_degree(
                                                    p.hz,
                                                    temp.divisions,
                                                    temp.period,
                                                    concert_hz,
                                                    key,
                                                    &scale.intervals,
                                                    50.0,
                                                )
                                            })
                                            .collect();
                                        (degs, vec![])
                                    }
                                } else {
                                    (vec![], vec![])
                                };
                            drop(state);

                            holdoff_degs.retain(|_, c| { if *c == 0 { false } else { *c -= 1; true } });
                            holdoff_steps.retain(|_, c| { if *c == 0 { false } else { *c -= 1; true } });
                            for &d in &fresh_degs { holdoff_degs.insert(d, HOLDOFF_FRAMES); }
                            for &s in &fresh_steps { holdoff_steps.insert(s, HOLDOFF_FRAMES); }
                            mic_degrees.set(holdoff_degs.keys().copied().collect());
                            mic_steps.set(holdoff_steps.keys().copied().collect());
                        }
                        pitch_tracking::MicEvent::Error(_) | pitch_tracking::MicEvent::Stopped => {
                            mic_active.set(false);
                            mic_degrees.set(vec![]);
                            mic_steps.set(vec![]);
                            break;
                        }
                        _ => {}
                    }
                }
            });
        }
    };

    use_effect(move || {
        spawn(async move {
            let mut e = eval(r#"
                function sendOrientation() {
                    dioxus.send({type: 'orientation', mobile: window.innerWidth <= 768, landscape: window.innerWidth > window.innerHeight});
                }
                sendOrientation();
                window.addEventListener('resize', sendOrientation);
                window.addEventListener('orientationchange', sendOrientation);
                // Swipe on drawer handle
                let startY = 0;
                document.addEventListener('touchstart', function(e) {
                    if (e.target.closest('.drawer-handle')) startY = e.touches[0].clientY;
                }, {passive: true});
                document.addEventListener('touchend', function(e) {
                    if (!e.target.closest('.drawer-handle')) return;
                    var delta = e.changedTouches[0].clientY - startY;
                    if (delta < -50) dioxus.send({type: 'swipe', dir: 'open'});
                    else if (delta > 50) dioxus.send({type: 'swipe', dir: 'close'});
                }, {passive: true});
            "#);
            loop {
                match e.recv().await {
                    Ok(serde_json::Value::Object(obj)) => {
                        match obj.get("type").and_then(|t| t.as_str()) {
                            Some("orientation") => {
                                let mobile = obj.get("mobile").and_then(|v| v.as_bool()).unwrap_or(false);
                                let landscape = obj.get("landscape").and_then(|v| v.as_bool()).unwrap_or(false);
                                is_mobile.set(mobile);
                                is_landscape.set(landscape);
                            }
                            Some("swipe") => {
                                match obj.get("dir").and_then(|v| v.as_str()) {
                                    Some("open") => drawer_open.set(true),
                                    Some("close") => drawer_open.set(false),
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => break,
                }
            }
        });
    });

    let effective_degrees = {
        let mut degs = playing_degrees.read().clone();
        degs.extend(mic_degrees.read().iter().copied());
        degs
    };
    let effective_steps = mic_steps.read().clone();

    rsx! {
        div { class: if effective_horizontal { "home-layout home-layout--h" } else { "home-layout" },

            // ── Controls panel ──────────────────────────────────────────────
            div {
                class: if *drawer_open.read() { "controls-panel drawer-open" } else { "controls-panel" },

                // Drawer handle — visible on mobile only
                div {
                    class: "drawer-handle",
                    onclick: move |_| {
                        let open = *drawer_open.read();
                        drawer_open.set(!open);
                    },
                    div { class: "drawer-pill" }
                    span { class: "drawer-label",
                        if *drawer_open.read() { "Settings ▾" } else { "Settings ▴" }
                    }
                }

                div { class: "controls-body",

                    // Instrument selector
                    div { class: "form-group",
                        label { class: "form-label", "Instrument" }
                        button {
                            class: "instrument-selector",
                            onclick: move |_| show_instrument_picker.set(true),
                            if has_instrument {
                                span { class: "instrument-selector-type", "{instrument_type_label}" }
                                span { class: "instrument-selector-label", "{instrument_label}" }
                            } else {
                                span { class: "instrument-selector-label instrument-selector-label--empty", "Select an instrument..." }
                            }
                            span { class: "instrument-selector-chevron",
                                dangerous_inner_html: r#"<svg width="12" height="12" viewBox="0 0 12 12" aria-hidden="true"><path fill="currentColor" d="M6 8L1 3h10z"/></svg>"#,
                            }
                        }
                    }

                    if has_instrument {
                        // Tuning
                        div { class: "form-group",
                            if !effective_horizontal { label { class: "form-label", "Tuning" } }
                            if tuning_options.is_empty() {
                                p { class: "form-empty", "No compatible tunings for this instrument." }
                            } else {
                                Select {
                                    value: sel_tuning_idx.to_string(),
                                    options: tuning_options.iter().map(|(idx, name)| (idx.to_string(), name.clone())).collect(),
                                    on_change: move |val: String| {
                                        if let Ok(idx) = val.parse::<usize>() {
                                            APP_STATE.write().selected_tuning_idx = idx;
                                        }
                                    },
                                }
                            }
                        }

                        // Notation system (only when multiple are available)
                        if notation_options.len() > 1 {
                            div { class: "form-group",
                                label { class: "form-label", "Notation" }
                                Select {
                                    value: current_ns_name.clone(),
                                    options: notation_options,
                                    on_change: {
                                        let temp_name = current_temp_name.clone();
                                        move |val: String| {
                                            APP_STATE.write().preferences.notation_system_prefs
                                                .insert(temp_name.clone(), val);
                                        }
                                    },
                                }
                            }
                        }

                        // Diagram type
                        div { class: "form-group",
                            if !effective_horizontal { label { class: "form-label", "Diagram Type" } }
                            div { class: "segmented-control",
                                button {
                                    class: if is_scale_mode { "seg-btn active" } else { "seg-btn" },
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.mode = DiagramMode::Scale;
                                    },
                                    "Scale"
                                }
                                button {
                                    class: if !is_scale_mode { "seg-btn active" } else { "seg-btn" },
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.mode = DiagramMode::Chord;
                                    },
                                    "Chord"
                                }
                            }
                        }

                        // Orientation
                        div { class: "form-group hide-mobile",
                            if !effective_horizontal { label { class: "form-label", "Orientation" } }
                            div { class: "segmented-control",
                                button {
                                    class: if !is_horizontal { "seg-btn active" } else { "seg-btn" },
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.horizontal = false;
                                    },
                                    "Vertical"
                                }
                                button {
                                    class: if is_horizontal { "seg-btn active" } else { "seg-btn" },
                                    onclick: move |_| {
                                        APP_STATE.write().diagram_settings.horizontal = true;
                                    },
                                    "Horizontal"
                                }
                            }
                        }

                        // Scale or Chord selection
                        if is_scale_mode {
                            div { class: "form-group",
                                label { class: "form-label", "Scale" }
                                if scale_options.is_empty() {
                                    p { class: "form-empty", "No scales defined for this temperament." }
                                } else {
                                    Combobox {
                                        value: sel_scale_idx.to_string(),
                                        options: scale_options.iter().map(|(idx, name)| (idx.to_string(), name.clone())).collect(),
                                        on_change: move |val: String| {
                                            if let Ok(idx) = val.parse::<usize>() {
                                                APP_STATE.write().selected_scale_idx = idx;
                                            }
                                        },
                                    }
                                }
                            }
                        } else {
                            div { class: "form-group",
                                label { class: "form-label", "Chord" }
                                if chord_options.is_empty() {
                                    p { class: "form-empty", "No chords defined for this temperament." }
                                } else {
                                    Combobox {
                                        value: sel_chord_idx.to_string(),
                                        options: chord_options.iter().map(|(idx, name)| (idx.to_string(), name.clone())).collect(),
                                        on_change: move |val: String| {
                                            if let Ok(idx) = val.parse::<usize>() {
                                                APP_STATE.write().selected_chord_idx = idx;
                                            }
                                        },
                                    }
                                }
                            }
                        }

                        // Key + Fret Offset (side by side)
                        div { class: "form-row",
                            div { class: "form-group",
                                label { class: "form-label", "Key" }
                                if let Some(ns) = notation_system.as_ref() {
                                    // Natural picker row
                                    div { class: "key-picker-naturals",
                                        for (nat_idx, nat) in ns.naturals.iter().enumerate() {
                                            {
                                                let nat_name = nat.name.clone();
                                                let is_selected = effective_nat_idx == Some(nat_idx);
                                                let is_smufl = nat_name.chars().any(|c| c as u32 >= 0xE000);
                                                rsx! {
                                                    button {
                                                        class: if is_selected { "key-btn key-btn--active" } else { "key-btn" },
                                                        style: if is_smufl { "font-family: 'Bravura', sans-serif;" } else { "" },
                                                        onclick: move |_| {
                                                            let mut s = APP_STATE.write();
                                                            s.diagram_settings.key_natural_idx = Some(nat_idx);
                                                            let temp = s.current_temperament().cloned();
                                                            if let Some(temp) = temp {
                                                                let ns = s.current_notation_system().cloned();
                                                                if let Some(ns) = ns {
                                                                    let nat = ns.naturals.get(nat_idx).cloned();
                                                                    let acc = s.diagram_settings.key_accidental_idx
                                                                        .and_then(|ai| ns.accidentals.get(ai).cloned());
                                                                    if let Some(nat) = nat {
                                                                        s.diagram_settings.key = ns.resolve_degree(
                                                                            &nat, acc.as_ref(), temp.divisions
                                                                        );
                                                                    }
                                                                }
                                                            }
                                                        },
                                                        "{nat_name}"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    // Accidental picker row
                                    div { class: "key-picker-accidentals",
                                        // Natural/no-accidental button
                                        {
                                            let is_selected = settings.key_accidental_idx.is_none();
                                            rsx! {
                                                button {
                                                    class: if is_selected { "key-btn key-btn--active" } else { "key-btn" },
                                                    style: "font-family: 'Bravura', sans-serif;",
                                                    onclick: move |_| {
                                                        let mut s = APP_STATE.write();
                                                        s.diagram_settings.key_accidental_idx = None;
                                                        let temp = s.current_temperament().cloned();
                                                        if let Some(temp) = temp {
                                                            let ns = s.current_notation_system().cloned();
                                                            if let Some(ns) = ns {
                                                                let key = s.diagram_settings.key;
                                                                let ni = s.diagram_settings.key_natural_idx
                                                                    .or_else(|| ns.naturals.iter().position(|n| n.degree == key))
                                                                    .unwrap_or(0);
                                                                s.diagram_settings.key_natural_idx = Some(ni);
                                                                if let Some(nat) = ns.naturals.get(ni) {
                                                                    s.diagram_settings.key = ns.resolve_degree(nat, None, temp.divisions);
                                                                }
                                                            }
                                                        }
                                                    },
                                                    "\u{E261}"
                                                }
                                            }
                                        }
                                        for (acc_idx, acc) in ns.accidentals.iter().enumerate() {
                                            {
                                                let acc_name = acc.name.clone();
                                                let is_selected = settings.key_accidental_idx == Some(acc_idx);
                                                let is_smufl = acc_name.chars().any(|c| c as u32 >= 0xE000);
                                                rsx! {
                                                    button {
                                                        class: if is_selected { "key-btn key-btn--active" } else { "key-btn" },
                                                        style: if is_smufl { "font-family: 'Bravura', sans-serif;" } else { "" },
                                                        onclick: move |_| {
                                                            let mut s = APP_STATE.write();
                                                            s.diagram_settings.key_accidental_idx = Some(acc_idx);
                                                            let temp = s.current_temperament().cloned();
                                                            if let Some(temp) = temp {
                                                                let ns = s.current_notation_system().cloned();
                                                                if let Some(ns) = ns {
                                                                    let key = s.diagram_settings.key;
                                                                    let ni = s.diagram_settings.key_natural_idx
                                                                        .or_else(|| ns.naturals.iter().position(|n| n.degree == key))
                                                                        .unwrap_or(0);
                                                                    s.diagram_settings.key_natural_idx = Some(ni);
                                                                    let acc = ns.accidentals.get(acc_idx).cloned();
                                                                    if let Some(nat) = ns.naturals.get(ni) {
                                                                        s.diagram_settings.key = ns.resolve_degree(
                                                                            nat, acc.as_ref(), temp.divisions
                                                                        );
                                                                    }
                                                                }
                                                            }
                                                        },
                                                        "{acc_name}"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    // Fallback: numeric input when no notation system
                                    input {
                                        class: "form-input",
                                        r#type: "number",
                                        min: "0",
                                        max: "{divisions - 1}",
                                        value: "{settings.key}",
                                        onchange: move |e| {
                                            if let Ok(v) = e.value().parse::<u32>() {
                                                APP_STATE.write().diagram_settings.key = v;
                                            }
                                        }
                                    }
                                }
                            }
                            div { class: "form-group",
                                label { class: "form-label", "Fret Offset" }
                                input {
                                    class: "form-input",
                                    r#type: "number",
                                    min: "0",
                                    value: "{settings.fret_offset}",
                                    onchange: move |e| {
                                        if let Ok(v) = e.value().parse::<u32>() {
                                            APP_STATE.write().diagram_settings.fret_offset = v;
                                        }
                                    }
                                }
                            }
                        }

                        // Concert pitch
                        div { class: "form-group",
                            label { class: "form-label", "Concert Pitch" }
                            div { class: "form-row",
                                div { class: "form-group",
                                    label { class: "form-label", "{concert_label} =" }
                                    input {
                                        class: "form-input",
                                        r#type: "number",
                                        step: "0.01",
                                        min: "1.0",
                                        max: "20000.0",
                                        value: "{concert_hz}",
                                        oninput: move |e| {
                                            if let Ok(v) = e.value().parse::<f64>() {
                                                if v > 0.0 {
                                                    APP_STATE.write().preferences.concert_hz = v;
                                                }
                                            }
                                        }
                                    }
                                    span { "Hz" }
                                }
                                div { class: "form-group",
                                    label { class: "form-label", "Octave" }
                                    input {
                                        class: "form-input",
                                        r#type: "number",
                                        min: "-2",
                                        max: "9",
                                        value: "{concert_octave}",
                                        oninput: move |e| {
                                            if let Ok(v) = e.value().parse::<i32>() {
                                                APP_STATE.write().preferences.concert_octave = v;
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // Action buttons
                        div { class: "controls-actions",
                            button {
                                class: "btn btn-outline btn-full",
                                onclick: export_svg,
                                "↓ Export SVG"
                            }
                        }
                    } else {
                        // Empty state when no instrument is selected
                        div { class: "empty-state instrument-empty-state",
                            span { class: "empty-icon", "🎸" }
                            p { "No instrument selected. Add an instrument to get started." }
                            button {
                                class: "btn btn-primary",
                                onclick: move |_| show_instrument_picker.set(true),
                                "Add Instrument"
                            }
                        }
                    }
                }
            }

            // ── Preview panel ───────────────────────────────────────────────
            div { class: "preview-panel",
                div { class: "preview-header",
                    div { class: "preview-header-info",
                        h2 { class: "preview-title",
                            {
                                let s = APP_STATE.read();
                                let temp_name = s.current_temperament()
                                    .map(|t| t.name.clone())
                                    .unwrap_or_default();
                                let item_name = if is_scale_mode {
                                    s.current_scale().map(|sc| sc.name.clone())
                                } else {
                                    s.current_chord().map(|ch| ch.name.clone())
                                }.unwrap_or_default();
                                format!("{} · {}", temp_name, item_name)
                            }
                        }
                        span { class: "preview-subtitle",
                            {
                                let s = APP_STATE.read();
                                s.current_instrument()
                                    .map(|i| {
                                        let tuning_name = s.current_tuning()
                                            .map(|t| t.name.clone())
                                            .unwrap_or_default();
                                        format!("{} — {}", i.name, tuning_name)
                                    })
                                    .unwrap_or_default()
                            }
                        }
                    }
                    div { class: "preview-header-actions",
                        if has_freqs {
                            button {
                                class: if is_playing() { "btn-play playing" } else { "btn-play" },
                                title: if is_playing() { "Playing…" } else { "Play scale" },
                                onclick: on_play,
                                if is_playing() { "■" } else { "▶" }
                            }
                        }
                        button {
                            class: if mic_active() { "btn-mic listening" } else { "btn-mic" },
                            title: if mic_active() { "Stop listening" } else { "Listen to mic" },
                            onclick: on_mic_toggle,
                            if mic_active() { "■" } else { "🎤" }
                        }
                    }
                }
                div { class: "preview-body",
                    FretboardPreview {
                        playing_degrees: effective_degrees.clone(),
                        playing_steps: effective_steps.clone(),
                        horizontal_override: Some(effective_horizontal),
                    }
                }
            }

        }

        // ── Instrument picker modal ─────────────────────────────────────
        InstrumentPicker { show: show_instrument_picker }
    }
}
