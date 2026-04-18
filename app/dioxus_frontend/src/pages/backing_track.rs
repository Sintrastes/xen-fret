use xen_sequencer::{audio, sequencer};
use xen_sequencer::sequencer::{Bar, ChordEntry, ChordSpec, Sequence, StrumDirection};
use crate::soundfont;
use crate::state::APP_STATE;
use dioxus::prelude::*;

// ── Groove definitions ──────────────────────────────────────────────────────

/// A single hit in a groove pattern.
#[derive(Debug, Clone, Copy)]
struct Hit {
    strum: StrumDirection,
    beats: f64,
}

/// A percussion hit within a groove cycle.
#[derive(Debug, Clone, Copy)]
struct DrumHit {
    /// Beat offset within the cycle (0-based).
    beat: f64,
    /// GM percussion note (channel 9 key).
    note: u8,
    /// Velocity 0.0–1.0.
    vel: f64,
}

// GM percussion note constants.
const KICK: u8 = 36;
const SNARE: u8 = 38;
const CLOSED_HH: u8 = 42;
const OPEN_HH: u8 = 46;
const RIDE: u8 = 51;
const CROSS_STICK: u8 = 37;
const PEDAL_HH: u8 = 44;

/// A groove is a repeating rhythmic pattern with optional drums.
#[derive(Debug, Clone)]
struct Groove {
    name: &'static str,
    /// Duration of one cycle in beats.
    cycle_beats: f64,
    /// Guitar strum hits within one cycle.
    hits: Vec<Hit>,
    /// Drum hits within one cycle (empty = no drums).
    drums: Vec<DrumHit>,
}

fn h(strum: StrumDirection, beats: f64) -> Hit {
    Hit { strum, beats }
}

fn d(beat: f64, note: u8, vel: f64) -> DrumHit {
    DrumHit { beat, note, vel }
}

fn grooves() -> Vec<Groove> {
    use StrumDirection::*;
    vec![
        Groove {
            name: "Whole",
            cycle_beats: 4.0,
            hits: vec![h(Simultaneous, 4.0)],
            drums: vec![],
        },
        Groove {
            name: "Pop Ballad",
            cycle_beats: 2.0,
            hits: vec![h(Down, 1.0), h(Up, 0.5), h(Down, 0.5)],
            // Kick on 1, snare on 2, hi-hat eighths
            drums: vec![
                d(0.0, KICK, 0.9), d(0.0, CLOSED_HH, 0.6),
                d(0.5, CLOSED_HH, 0.4),
                d(1.0, SNARE, 0.8), d(1.0, CLOSED_HH, 0.6),
                d(1.5, CLOSED_HH, 0.4),
            ],
        },
        Groove {
            name: "Pop Eighths",
            cycle_beats: 2.0,
            hits: vec![h(Down, 0.5), h(Up, 0.5), h(Down, 0.5), h(Up, 0.5)],
            // Kick on 1, snare on 2, hi-hat eighths
            drums: vec![
                d(0.0, KICK, 0.8), d(0.0, CLOSED_HH, 0.6),
                d(0.5, CLOSED_HH, 0.4),
                d(1.0, SNARE, 0.8), d(1.0, CLOSED_HH, 0.6),
                d(1.5, CLOSED_HH, 0.4),
            ],
        },
        Groove {
            name: "Rock Driving",
            cycle_beats: 2.0,
            hits: vec![h(Down, 0.5), h(Down, 0.5), h(Up, 0.5), h(Down, 0.5)],
            // Kick on 1, snare on 2, driving hi-hat
            drums: vec![
                d(0.0, KICK, 1.0), d(0.0, CLOSED_HH, 0.7),
                d(0.5, CLOSED_HH, 0.5),
                d(1.0, SNARE, 0.9), d(1.0, CLOSED_HH, 0.7),
                d(1.5, CLOSED_HH, 0.5),
            ],
        },
        Groove {
            name: "Rock Power",
            cycle_beats: 2.0,
            hits: vec![h(Down, 1.0), h(Down, 0.5), h(Up, 0.5)],
            // Heavy kick-snare with open hi-hat
            drums: vec![
                d(0.0, KICK, 1.0), d(0.0, CLOSED_HH, 0.7),
                d(0.5, CLOSED_HH, 0.5),
                d(1.0, SNARE, 1.0), d(1.0, OPEN_HH, 0.8),
                d(1.5, PEDAL_HH, 0.5),
            ],
        },
        Groove {
            name: "Blues Shuffle",
            cycle_beats: 2.0,
            hits: vec![h(Down, 0.67), h(Up, 0.33), h(Down, 0.67), h(Up, 0.33)],
            // Swung ride + kick/snare
            drums: vec![
                d(0.0, KICK, 0.8), d(0.0, RIDE, 0.7),
                d(0.67, RIDE, 0.5),
                d(1.0, SNARE, 0.7), d(1.0, RIDE, 0.7),
                d(1.67, RIDE, 0.5),
            ],
        },
        Groove {
            name: "Jazz Comping",
            cycle_beats: 4.0,
            hits: vec![h(Simultaneous, 1.5), h(Simultaneous, 0.5), h(Simultaneous, 1.0), h(Simultaneous, 1.0)],
            // Ride cymbal swing pattern + kick/cross-stick accents
            drums: vec![
                d(0.0, RIDE, 0.7), d(0.0, KICK, 0.6),
                d(0.67, RIDE, 0.4),
                d(1.0, RIDE, 0.6),
                d(1.67, RIDE, 0.4),
                d(2.0, RIDE, 0.7), d(2.0, CROSS_STICK, 0.5),
                d(2.67, RIDE, 0.4),
                d(3.0, RIDE, 0.6),
                d(3.67, RIDE, 0.4),
            ],
        },
        Groove {
            name: "Fingerpick",
            cycle_beats: 1.0,
            hits: vec![h(Down, 1.0)],
            drums: vec![],
        },
    ]
}

/// Expand a groove to fill `beats_per_bar` beats, returning ChordEntry events
/// with the given chord parameters.
fn expand_groove(
    groove: &Groove,
    beats_per_bar: f64,
    scale_degree: u32,
    chord: &ChordSpec,
    octave_offset: i32,
) -> Vec<ChordEntry> {
    let mut entries = Vec::new();
    let mut remaining = beats_per_bar;
    while remaining > 0.01 {
        for hit in &groove.hits {
            if remaining < 0.01 { break; }
            let dur = hit.beats.min(remaining);
            entries.push(ChordEntry {
                scale_degree,
                chord: chord.clone(),
                strum: hit.strum,
                duration_beats: dur,
                octave_offset,
            });
            remaining -= dur;
        }
    }
    entries
}

// ── Page-local types ────────────────────────────────────────────────────────

/// One bar: a chord + a groove.
#[derive(Debug, Clone, PartialEq)]
struct TrackBar {
    scale_degree: u32,
    chord_name: String,
    chord_spec: ChordSpec,
    octave_offset: i32,
    groove_idx: usize,
}

#[derive(Debug, Clone, PartialEq)]
struct TrackState {
    beats_per_bar: u32,
    bpm: f64,
    temperament_name: String,
    scale_intervals: Vec<i32>,
    key: i32,
    octave: i32,
    divisions: u32,
    period: (u32, u32),
    bars: Vec<TrackBar>,
}

/// Which field in a bar is being edited.
#[derive(Debug, Clone, Copy, PartialEq)]
enum EditField {
    Chord,
    Groove,
}

// ── Default track ───────────────────────────────────────────────────────────

fn default_track() -> TrackState {
    // Pop/rock progression: I - V - vi - IV in D major
    let bars = vec![
        TrackBar { scale_degree: 1, chord_name: "D".into(),  chord_spec: ChordSpec::Named("Major".into()), octave_offset: 0, groove_idx: 1 },
        TrackBar { scale_degree: 5, chord_name: "A".into(),  chord_spec: ChordSpec::Named("Major".into()), octave_offset: 0, groove_idx: 1 },
        TrackBar { scale_degree: 6, chord_name: "Bm".into(), chord_spec: ChordSpec::Named("Minor".into()), octave_offset: 0, groove_idx: 1 },
        TrackBar { scale_degree: 4, chord_name: "G".into(),  chord_spec: ChordSpec::Named("Major".into()), octave_offset: 0, groove_idx: 1 },
        TrackBar { scale_degree: 1, chord_name: "D".into(),  chord_spec: ChordSpec::Named("Major".into()), octave_offset: 0, groove_idx: 2 },
        TrackBar { scale_degree: 5, chord_name: "A".into(),  chord_spec: ChordSpec::Named("Major".into()), octave_offset: 0, groove_idx: 2 },
        TrackBar { scale_degree: 6, chord_name: "Bm".into(), chord_spec: ChordSpec::Named("Minor".into()), octave_offset: 0, groove_idx: 2 },
        TrackBar { scale_degree: 4, chord_name: "G".into(),  chord_spec: ChordSpec::Named("Major".into()), octave_offset: 0, groove_idx: 2 },
    ];
    TrackState {
        beats_per_bar: 4,
        bpm: 100.0,
        temperament_name: "12-TET".into(),
        scale_intervals: vec![2, 2, 1, 2, 2, 2, 1],
        key: 5,
        octave: -2,
        divisions: 12,
        period: (2, 1),
        bars,
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────────

fn preview_bar(ts: &TrackState, bar: &TrackBar) {
    let mini = Sequence {
        temperament_name: ts.temperament_name.clone(),
        scale_intervals: ts.scale_intervals.clone(),
        key: ts.key,
        octave: ts.octave,
        divisions: ts.divisions,
        period: ts.period,
        bpm: 120.0,
        beats_per_bar: 4,
        bars: vec![Bar {
            events: vec![ChordEntry {
                scale_degree: bar.scale_degree,
                chord: bar.chord_spec.clone(),
                strum: StrumDirection::Simultaneous,
                duration_beats: 4.0,
                octave_offset: bar.octave_offset,
            }],
        }],
    };
    let concert_hz = APP_STATE.read().preferences.concert_hz;
    let chords: Vec<_> = APP_STATE.read().chords.iter().cloned().collect();
    let timeline = sequencer::compile(&mini, concert_hz, &chords);

    // Try soundfont rendering for richer chord preview.
    if let Some(sf) = soundfont::get_soundfont() {
        if let Some(wav) = soundfont::render_basic_timeline(
            sf,
            &timeline,
            soundfont::GM_ACOUSTIC_GUITAR_NYLON,
        ) {
            let dur = timeline.total_duration_s;
            spawn(async move { audio::play_wav_bytes(&wav, dur).await; });
            return;
        }
    }

    // Fallback: JS chord playback.
    let freqs: Vec<f64> = timeline.events.iter().map(|e| e.freq_hz).collect();
    if !freqs.is_empty() {
        spawn(async move { audio::play_chord(&freqs).await; });
    }
}

/// Build an InstrumentTimeline with guitar on channel 0 and drums on channel 9.
fn build_instrument_timeline(ts: &TrackState) -> Option<soundfont::InstrumentTimeline> {
    let concert_hz = APP_STATE.read().preferences.concert_hz;
    let chords: Vec<_> = APP_STATE.read().chords.iter().cloned().collect();
    let seq = to_sequence(ts);
    let note_timeline = sequencer::compile(&seq, concert_hz, &chords);

    let beat_s = 60.0 / ts.bpm;
    let all_grooves = grooves();
    let mut events = Vec::new();

    // Guitar events from the compiled NoteTimeline.
    for e in &note_timeline.events {
        events.push(soundfont::InstrumentEvent {
            start_s: e.start_s,
            duration_s: e.duration_s,
            freq_hz: e.freq_hz,
            velocity: e.velocity,
            program: soundfont::GM_ACOUSTIC_GUITAR_NYLON,
            channel: 0,
        });
    }

    // Drum events: expand each bar's groove drum pattern.
    let bar_duration_s = ts.beats_per_bar as f64 * beat_s;
    for (bar_idx, bar) in ts.bars.iter().enumerate() {
        let groove = all_grooves.get(bar.groove_idx).unwrap_or(&all_grooves[0]);
        if groove.drums.is_empty() {
            continue;
        }
        let bar_start = bar_idx as f64 * bar_duration_s;
        let mut beat_cursor = 0.0;
        while beat_cursor < ts.beats_per_bar as f64 - 0.01 {
            for dh in &groove.drums {
                let abs_beat = beat_cursor + dh.beat;
                if abs_beat >= ts.beats_per_bar as f64 {
                    break;
                }
                events.push(soundfont::InstrumentEvent {
                    start_s: bar_start + abs_beat * beat_s,
                    duration_s: 0.15,
                    freq_hz: 440.0 * 2.0f64.powf((dh.note as f64 - 69.0) / 12.0),
                    velocity: dh.vel,
                    program: 0, // ignored for channel 9
                    channel: 9,
                });
            }
            beat_cursor += groove.cycle_beats;
        }
    }

    let total_duration_s = note_timeline.total_duration_s;
    Some(soundfont::InstrumentTimeline { events, total_duration_s })
}

fn to_sequence(ts: &TrackState) -> Sequence {
    let all_grooves = grooves();
    Sequence {
        temperament_name: ts.temperament_name.clone(),
        scale_intervals: ts.scale_intervals.clone(),
        key: ts.key,
        octave: ts.octave,
        divisions: ts.divisions,
        period: ts.period,
        bpm: ts.bpm,
        beats_per_bar: ts.beats_per_bar,
        bars: ts.bars.iter().map(|b| {
            let groove = all_grooves.get(b.groove_idx).unwrap_or(&all_grooves[0]);
            Bar {
                events: expand_groove(groove, ts.beats_per_bar as f64, b.scale_degree, &b.chord_spec, b.octave_offset),
            }
        }).collect(),
    }
}

// ── Component ───────────────────────────────────────────────────────────────

#[component]
pub fn BackingTrack() -> Element {
    let mut track = use_signal(default_track);
    let mut editing: Signal<Option<(usize, EditField)>> = use_signal(|| None);
    let mut is_playing = use_signal(|| false);
    let mut metronome = use_signal(|| false);
    let all_grooves = grooves();

    let on_play = move |_| {
        if is_playing() {
            audio::stop_all();
            is_playing.set(false);
            return;
        }
        spawn(async move {
            is_playing.set(true);
            let ts = track.read().clone();

            // Try soundfont rendering with drums; fall back to JS sample engine.
            if let Some(sf) = soundfont::get_soundfont() {
                if let Some(inst_tl) = build_instrument_timeline(&ts) {
                    if let Some(wav) = soundfont::render_timeline(sf, &inst_tl) {
                        audio::play_wav_bytes(&wav, inst_tl.total_duration_s).await;
                        is_playing.set(false);
                        return;
                    }
                }
            }

            // Fallback: JS-based playback (no drums).
            let seq = to_sequence(&ts);
            let concert_hz = APP_STATE.read().preferences.concert_hz;
            let chords: Vec<_> = APP_STATE.read().chords.iter().cloned().collect();
            let timeline = sequencer::compile(&seq, concert_hz, &chords);
            let metro = if metronome() {
                Some((seq.bpm, seq.beats_per_bar))
            } else {
                None
            };
            audio::play_timeline(&timeline, metro).await;
            is_playing.set(false);
        });
    };

    let mut add_bar = move |_: Event<MouseData>| {
        track.write().bars.push(TrackBar {
            scale_degree: 1,
            chord_name: "Maj".into(),
            chord_spec: ChordSpec::Named("Major".into()),
            octave_offset: 0,
            groove_idx: 1,
        });
    };

    rsx! {
        div { class: "page backing-track-page",
            onclick: move |_| { editing.set(None); },

            div { class: "page-header",
                h1 { "Backing Track" }
            }

            // ── Controls ────────────────────────────────────────────────
            div { class: "card track-controls",
                onclick: move |evt: Event<MouseData>| { evt.stop_propagation(); },
                div { class: "form-group track-control-item",
                    label { "Time Sig" }
                    select {
                        class: "form-select",
                        value: format!("{}", track.read().beats_per_bar),
                        onchange: move |evt: Event<FormData>| {
                            if let Ok(v) = evt.value().parse::<u32>() {
                                track.write().beats_per_bar = v;
                            }
                        },
                        option { value: "2", "2/4" }
                        option { value: "3", "3/4" }
                        option { value: "4", "4/4" }
                        option { value: "6", "6/8" }
                    }
                }
                div { class: "form-group track-control-item",
                    label { "BPM" }
                    input {
                        class: "form-input",
                        r#type: "number",
                        min: "20",
                        max: "300",
                        value: format!("{}", track.read().bpm as u32),
                        onchange: move |evt: Event<FormData>| {
                            if let Ok(v) = evt.value().parse::<f64>() {
                                track.write().bpm = v.clamp(20.0, 300.0);
                            }
                        },
                    }
                }
                div { class: "form-group track-control-item",
                    label { "Temperament" }
                    select {
                        class: "form-select",
                        value: "{track.read().temperament_name}",
                        onchange: move |evt: Event<FormData>| {
                            let name = evt.value();
                            let (seq, _) = sequencer::komm_susser_tod_demo(&name);
                            let mut t = track.write();
                            t.temperament_name = seq.temperament_name;
                            t.scale_intervals = seq.scale_intervals;
                            t.key = seq.key;
                            t.divisions = seq.divisions;
                        },
                        option { value: "12-TET", "12-TET" }
                        option { value: "22-TET", "22-TET" }
                        option { value: "31-TET", "31-TET" }
                    }
                }
                div { class: "track-control-buttons",
                    button {
                        class: if metronome() { "btn-play playing" } else { "btn-play" },
                        title: if metronome() { "Metronome on" } else { "Metronome off" },
                        onclick: move |_| { metronome.set(!metronome()); },
                        span {
                            dangerous_inner_html: r#"<svg width="17" height="17" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M7 21h10L14.5 4h-5L7 21z"/><line x1="8.8" y1="15" x2="15.2" y2="15"/><line x1="12" y1="4.5" x2="17.5" y2="14"/><circle cx="12" cy="4.5" r="1.4" fill="currentColor" stroke="none"/></svg>"#,
                        }
                    }
                    button {
                        class: if is_playing() { "btn-play playing" } else { "btn btn-primary" },
                        onclick: on_play,
                        if is_playing() { "\u{25A0} Stop" } else { "\u{25B6} Play" }
                    }
                }
            }

            // ── Score ───────────────────────────────────────────────────
            div { class: "card track-sheet-card",
                onclick: move |evt: Event<MouseData>| { evt.stop_propagation(); },
                div { class: "track-sheet",
                    for (bar_idx, bar) in track.read().bars.iter().enumerate() {
                        {
                            let bi = bar_idx;
                            let cur = editing();
                            let bar_name = bar.chord_name.clone();
                            let bar_degree = bar.scale_degree;
                            let bar_groove = bar.groove_idx;
                            let bar_oct = bar.octave_offset;
                            let groove_name = all_grooves.get(bar_groove).map(|g| g.name).unwrap_or("?");
                            let is_active = cur.map(|(b, _)| b) == Some(bi);

                            rsx! {
                                div {
                                    class: if is_active { "track-bar active" } else { "track-bar" },
                                    "data-bar": format!("{}", bi + 1),

                                    // ── Chord cell ──────────────
                                    if cur == Some((bi, EditField::Chord)) {
                                        div {
                                            class: "track-bar-edit",
                                            onclick: move |evt: Event<MouseData>| { evt.stop_propagation(); },
                                            div { class: "track-edit-row",
                                                label { "Degree" }
                                                input {
                                                    class: "form-input track-edit-input",
                                                    r#type: "number",
                                                    min: "1", max: "14",
                                                    value: format!("{bar_degree}"),
                                                    onchange: move |evt: Event<FormData>| {
                                                        if let Ok(v) = evt.value().parse::<u32>() {
                                                            track.write().bars[bi].scale_degree = v.max(1);
                                                            preview_bar(&track.read(), &track.read().bars[bi]);
                                                        }
                                                    },
                                                }
                                            }
                                            div { class: "track-edit-row",
                                                label { "Name" }
                                                input {
                                                    class: "form-input track-edit-input",
                                                    value: "{bar_name}",
                                                    onchange: move |evt: Event<FormData>| {
                                                        track.write().bars[bi].chord_name = evt.value();
                                                    },
                                                }
                                            }
                                            div { class: "track-edit-row",
                                                label { "Oct" }
                                                input {
                                                    class: "form-input track-edit-input",
                                                    r#type: "number",
                                                    min: "-3", max: "3",
                                                    value: format!("{bar_oct}"),
                                                    onchange: move |evt: Event<FormData>| {
                                                        if let Ok(v) = evt.value().parse::<i32>() {
                                                            track.write().bars[bi].octave_offset = v;
                                                            preview_bar(&track.read(), &track.read().bars[bi]);
                                                        }
                                                    },
                                                }
                                            }
                                        }
                                    } else {
                                        div {
                                            class: "track-bar-chord",
                                            onclick: move |evt: Event<MouseData>| {
                                                evt.stop_propagation();
                                                editing.set(Some((bi, EditField::Chord)));
                                                preview_bar(&track.read(), &track.read().bars[bi]);
                                            },
                                            span { class: "chord-symbol", "{bar_name}" }
                                        }
                                    }

                                    // ── Groove cell ─────────────
                                    if cur == Some((bi, EditField::Groove)) {
                                        div {
                                            class: "track-bar-edit",
                                            onclick: move |evt: Event<MouseData>| { evt.stop_propagation(); },
                                            select {
                                                class: "form-select track-groove-select",
                                                value: format!("{bar_groove}"),
                                                onchange: move |evt: Event<FormData>| {
                                                    if let Ok(v) = evt.value().parse::<usize>() {
                                                        track.write().bars[bi].groove_idx = v;
                                                    }
                                                    editing.set(None);
                                                },
                                                for (gi, g) in grooves().iter().enumerate() {
                                                    option {
                                                        value: format!("{gi}"),
                                                        "{g.name}"
                                                    }
                                                }
                                            }
                                        }
                                    } else {
                                        div {
                                            class: "track-bar-groove",
                                            onclick: move |evt: Event<MouseData>| {
                                                evt.stop_propagation();
                                                editing.set(Some((bi, EditField::Groove)));
                                            },
                                            "{groove_name}"
                                        }
                                    }

                                    // ── Remove button ───────────
                                    if track.read().bars.len() > 1 {
                                        button {
                                            class: "bar-remove-btn",
                                            title: "Remove bar",
                                            onclick: move |evt: Event<MouseData>| {
                                                evt.stop_propagation();
                                                track.write().bars.remove(bi);
                                                editing.set(None);
                                            },
                                            "\u{00D7}"
                                        }
                                    }
                                }
                            }
                        }
                    }

                    button {
                        class: "bar-add-btn",
                        title: "Add bar",
                        onclick: move |evt: Event<MouseData>| {
                            evt.stop_propagation();
                            add_bar(evt);
                        },
                        "+"
                    }
                }
            }
        }
    }
}
