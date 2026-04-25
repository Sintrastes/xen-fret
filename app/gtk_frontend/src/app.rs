use app_common::{
    hit_test::hit_test,
    state::{AppState, DiagramMode},
    storage,
};

static SF2_BYTES: &[u8] = include_bytes!(
    "../../dioxus_frontend/assets/soundfont.sf2"
);
use fretboard_diagrams::DiagramLayout;
use glib::SignalHandlerId;
use relm4::{
    adw::{self, prelude::*},
    gtk::{self, glib},
    ComponentParts, ComponentSender, SimpleComponent,
};
use xen_theory::instrument::{default_instrument_types, default_string_count, Instrument};

pub struct AppModel {
    state: AppState,
    instrument_names: Vec<String>,
    scale_names: Vec<String>,
    chord_names: Vec<String>,
    tuning_names: Vec<String>,
    pending_texture: std::cell::RefCell<Option<(Vec<u8>, u32, u32)>>,
    /// Layout from the last completed render; used for click hit-testing.
    diagram_layout: Option<DiagramLayout>,
    /// Absolute EDO steps currently flashing red from user clicks.
    flash_steps: Vec<i32>,
    needs_render: std::cell::Cell<bool>,
    /// Set when instrument/scale/tuning/key state changes so update_view repopulates
    /// dropdowns and spinners.  Not set for mic/flash/render-only updates so that
    /// those never call set_model on open dropdowns and steal focus.
    needs_ui_sync: std::cell::Cell<bool>,
    audio: Option<xen_sequencer::native_audio::NativeAudioBackend>,
    mic_handle: Option<std::sync::Arc<xen_sequencer::native_mic::NativeMicHandle>>,
    mic_steps: Vec<i32>,
}

#[derive(Debug)]
pub enum AppMsg {
    InstrumentSelected(u32),
    ScaleOrChordSelected(u32),
    TuningSelected(u32),
    ModeSet(DiagramMode),
    KeyChanged(f64),
    FretOffsetChanged(f64),
    DiagramReady(Vec<u8>, u32, u32, DiagramLayout),
    AddInstrumentConfirmed(Instrument),
    /// User clicked the diagram widget at widget-local (x, y); widget size given.
    DiagramClicked { x: f64, y: f64, widget_w: f64, widget_h: f64 },
    /// Flash timer expired for a specific absolute step.
    DiagramFlashExpired(i32),
    /// Mic listen toggle button clicked.
    MicToggle,
    /// Detector thread emitted new matched steps.
    MicPitchesChanged(Vec<i32>),
    /// Mic stream stopped or errored.
    MicStopped,
}

impl AppModel {
    fn rebuild_derived(&mut self) {
        self.instrument_names = self
            .state
            .instruments
            .iter()
            .map(|i| i.name.clone())
            .collect();
        let tname = self
            .state
            .current_temperament()
            .map(|t| t.name.clone())
            .unwrap_or_default();
        self.scale_names = self
            .state
            .scales
            .iter()
            .filter(|s| s.temperament_name == tname)
            .map(|s| s.name.clone())
            .collect();
        self.chord_names = self
            .state
            .chords
            .iter()
            .filter(|c| c.temperament_name == tname)
            .map(|c| c.name.clone())
            .collect();
        self.tuning_names = self
            .state
            .compatible_tunings()
            .into_iter()
            .map(|(_, tu)| tu.name.clone())
            .collect();
    }

    fn repopulate(dd: &gtk::DropDown, signal: &SignalHandlerId, names: &[String], sel: usize) {
        dd.block_signal(signal);
        let strs: Vec<&str> = names.iter().map(|s| s.as_str()).collect();
        dd.set_model(Some(&gtk::StringList::new(&strs)));
        if sel < names.len() {
            dd.set_selected(sel as u32);
        }
        dd.unblock_signal(signal);
    }
}

// ── Widget struct (named fields for the view! macro) ─────────────────────────

pub struct AppWidgets {
    #[allow(dead_code)]
    window: adw::ApplicationWindow,
    instrument_dd: gtk::DropDown,
    instrument_sig: SignalHandlerId,
    sc_label: gtk::Label,
    sc_dd: gtk::DropDown,
    sc_sig: SignalHandlerId,
    tuning_dd: gtk::DropDown,
    tuning_sig: SignalHandlerId,
    key_spin: gtk::SpinButton,
    fret_spin: gtk::SpinButton,
    diagram: gtk::Picture,
    mic_btn: gtk::ToggleButton,
    mic_sig: SignalHandlerId,
}

// ── SimpleComponent impl ──────────────────────────────────────────────────────

impl SimpleComponent for AppModel {
    type Init = ();
    type Input = AppMsg;
    type Output = ();
    type Root = adw::ApplicationWindow;
    type Widgets = AppWidgets;

    fn init_root() -> Self::Root {
        adw::ApplicationWindow::new(&relm4::main_adw_application())
    }

    fn init(
        _: Self::Init,
        root: Self::Root,
        sender: ComponentSender<Self>,
    ) -> ComponentParts<Self> {
        let mut state = storage::load();
        if state.selected_instrument_idx.is_none() && !state.instruments.is_empty() {
            state.select_instrument(0);
        }
        let sf = xen_sequencer::native_audio::soundfont_from_bytes(SF2_BYTES);
        let audio = xen_sequencer::native_audio::NativeAudioBackend::new(sf).ok();
        let mut model = AppModel {
            state,
            instrument_names: vec![],
            scale_names: vec![],
            chord_names: vec![],
            tuning_names: vec![],
            pending_texture: std::cell::RefCell::new(None),
            diagram_layout: None,
            flash_steps: vec![],
            needs_render: std::cell::Cell::new(false),
            needs_ui_sync: std::cell::Cell::new(true),
            audio,
            mic_handle: None,
            mic_steps: vec![],
        };
        model.rebuild_derived();

        // ── Build widget tree ─────────────────────────────────────────────

        root.set_title(Some("Xen Fret"));
        root.set_default_size(1100, 700);

        // Header bar
        let header = adw::HeaderBar::new();
        let title = adw::WindowTitle::new("Xen Fret", "");
        header.set_title_widget(Some(&title));

        // Toggle button shown only in narrow (bottom-sheet) mode
        let controls_btn = gtk::ToggleButton::builder()
            .icon_name("view-sidebar-symbolic")
            .active(true)
            .visible(false) // breakpoint will make it visible
            .build();
        header.pack_end(&controls_btn);

        // Mic listen toggle (always visible)
        let mic_btn = gtk::ToggleButton::builder()
            .icon_name("audio-input-microphone-symbolic")
            .tooltip_text("Listen to mic")
            .build();
        let mic_sig = {
            let s = sender.clone();
            mic_btn.connect_toggled(move |_| { s.input(AppMsg::MicToggle); })
        };
        header.pack_start(&mic_btn);

        // ── Sidebar widgets ───────────────────────────────────────────────

        let instrument_label = gtk::Label::builder()
            .label("Instrument")
            .halign(gtk::Align::Start)
            .hexpand(true)
            .build();
        let add_instrument_btn = gtk::Button::builder()
            .icon_name("list-add-symbolic")
            .tooltip_text("Add instrument")
            .css_classes(["flat", "circular"])
            .build();
        let instrument_header = gtk::Box::builder()
            .orientation(gtk::Orientation::Horizontal)
            .build();
        instrument_header.append(&instrument_label);
        instrument_header.append(&add_instrument_btn);

        let instrument_dd = gtk::DropDown::builder().hexpand(true).build();

        let sep1 = gtk::Separator::new(gtk::Orientation::Horizontal);

        // Mode toggle (Scale / Chord) as a linked button group
        let scale_btn = gtk::ToggleButton::builder()
            .label("Scale")
            .active(true)
            .build();
        let chord_btn = gtk::ToggleButton::builder().label("Chord").build();
        chord_btn.set_group(Some(&scale_btn));

        let mode_box = gtk::Box::builder()
            .orientation(gtk::Orientation::Horizontal)
            .homogeneous(true)
            .css_classes(["linked"])
            .build();
        mode_box.append(&scale_btn);
        mode_box.append(&chord_btn);

        {
            let s = sender.clone();
            scale_btn.connect_toggled(move |b| {
                if b.is_active() {
                    s.input(AppMsg::ModeSet(DiagramMode::Scale));
                }
            });
        }
        {
            let s = sender.clone();
            chord_btn.connect_toggled(move |b| {
                if b.is_active() {
                    s.input(AppMsg::ModeSet(DiagramMode::Chord));
                }
            });
        }

        let sep2 = gtk::Separator::new(gtk::Orientation::Horizontal);

        let sc_label = gtk::Label::builder()
            .label("Scale")
            .halign(gtk::Align::Start)
            .build();
        let sc_dd = gtk::DropDown::builder().hexpand(true).build();

        let tuning_label = gtk::Label::builder()
            .label("Tuning")
            .halign(gtk::Align::Start)
            .build();
        let tuning_dd = gtk::DropDown::builder().hexpand(true).build();

        let sep3 = gtk::Separator::new(gtk::Orientation::Horizontal);

        let key_label = gtk::Label::builder()
            .label("Key")
            .halign(gtk::Align::Start)
            .build();
        let key_spin = gtk::SpinButton::builder()
            .hexpand(true)
            .digits(0)
            .snap_to_ticks(true)
            .build();

        let fret_label = gtk::Label::builder()
            .label("Fret Offset")
            .halign(gtk::Align::Start)
            .build();
        let fret_spin = gtk::SpinButton::builder()
            .hexpand(true)
            .digits(0)
            .snap_to_ticks(true)
            .build();

        // Assemble sidebar box
        let sidebar_box = gtk::Box::builder()
            .orientation(gtk::Orientation::Vertical)
            .spacing(8)
            .margin_top(12)
            .margin_bottom(12)
            .margin_start(12)
            .margin_end(12)
            .build();
        sidebar_box.append(&instrument_header);
        sidebar_box.append(&instrument_dd);
        sidebar_box.append(&sep1);
        sidebar_box.append(&mode_box);
        sidebar_box.append(&sep2);
        sidebar_box.append(&sc_label);
        sidebar_box.append(&sc_dd);
        sidebar_box.append(&tuning_label);
        sidebar_box.append(&tuning_dd);
        sidebar_box.append(&sep3);
        sidebar_box.append(&key_label);
        sidebar_box.append(&key_spin);
        sidebar_box.append(&fret_label);
        sidebar_box.append(&fret_spin);

        let sidebar_scroll = gtk::ScrolledWindow::builder()
            .width_request(240)
            .min_content_height(300)
            .hscrollbar_policy(gtk::PolicyType::Never)
            .vscrollbar_policy(gtk::PolicyType::Automatic)
            .child(&sidebar_box)
            .build();

        // ── Diagram area ──────────────────────────────────────────────────

        let diagram = gtk::Picture::builder()
            .hexpand(true)
            .vexpand(true)
            .can_shrink(true)
            .content_fit(gtk::ContentFit::Contain)
            .build();

        // ── Diagram click handling ────────────────────────────────────────
        let gesture = gtk::GestureClick::new();
        gesture.connect_pressed(glib::clone!(
            #[strong]
            sender,
            #[strong]
            diagram,
            move |_g, _n, x, y| {
                let widget_w = diagram.width() as f64;
                let widget_h = diagram.height() as f64;
                sender.input(AppMsg::DiagramClicked { x, y, widget_w, widget_h });
            }
        ));
        diagram.add_controller(gesture);

        let diagram_box = gtk::Box::builder()
            .orientation(gtk::Orientation::Vertical)
            .hexpand(true)
            .vexpand(true)
            .margin_top(32)
            .margin_bottom(32)
            .build();
        diagram_box.append(&diagram);

        // ── Adaptive layout ───────────────────────────────────────────────
        //
        // "sidebar" (wide): OverlaySplitView — controls panel on the left,
        //                   diagram fills the rest.
        // "bottom-sheet" (narrow): BottomSheet — diagram full-screen,
        //                          controls in a draggable sheet.
        //
        // Both layouts reference the same widgets via named LayoutSlots.

        let multi_layout = adw::MultiLayoutView::new();

        // Sidebar layout
        let sidebar_layout_content = adw::OverlaySplitView::new();
        sidebar_layout_content.set_sidebar_position(gtk::PackType::Start);
        sidebar_layout_content.set_sidebar(Some(&adw::LayoutSlot::new("secondary")));
        sidebar_layout_content.set_content(Some(&adw::LayoutSlot::new("primary")));
        let sidebar_layout = adw::Layout::new(&sidebar_layout_content);
        sidebar_layout.set_name(Some("sidebar"));
        multi_layout.add_layout(sidebar_layout);

        // Bottom-sheet layout
        let sheet_layout_content = adw::BottomSheet::new();
        sheet_layout_content.set_open(true);
        sheet_layout_content.set_content(Some(&adw::LayoutSlot::new("primary")));
        sheet_layout_content.set_sheet(Some(&adw::LayoutSlot::new("secondary")));
        let sheet_layout = adw::Layout::new(&sheet_layout_content);
        sheet_layout.set_name(Some("bottom-sheet"));
        multi_layout.add_layout(sheet_layout);

        // Assign actual widgets to the named slots
        multi_layout.set_child("primary", &diagram_box);
        multi_layout.set_child("secondary", &sidebar_scroll);

        // Breakpoint: switch to bottom-sheet + show Controls button at ≤680sp
        let bp_condition = adw::BreakpointCondition::parse("max-width: 680sp")
            .expect("valid breakpoint condition");
        let breakpoint = adw::Breakpoint::new(bp_condition);
        breakpoint.add_setter(
            &multi_layout,
            "layout-name",
            Some(&"bottom-sheet".to_value()),
        );
        breakpoint.add_setter(&controls_btn, "visible", Some(&true.to_value()));
        root.add_breakpoint(breakpoint);

        // Controls button ↔ BottomSheet open property (bidirectional)
        controls_btn.connect_toggled(glib::clone!(
            #[strong]
            sheet_layout_content,
            move |btn| {
                sheet_layout_content.set_open(btn.is_active());
            }
        ));
        sheet_layout_content.connect_open_notify(glib::clone!(
            #[strong]
            controls_btn,
            move |sheet| {
                controls_btn.set_active(sheet.is_open());
            }
        ));

        // ── Toolbar view ──────────────────────────────────────────────────

        let toolbar_view = adw::ToolbarView::new();
        toolbar_view.add_top_bar(&header);
        toolbar_view.set_content(Some(&multi_layout));

        root.set_content(Some(&toolbar_view));

        // ── Add-instrument dialog ─────────────────────────────────────────

        {
            let root_ref = root.clone();
            let temperament_names: Vec<String> = model
                .state
                .temperaments
                .iter()
                .map(|t| t.name.clone())
                .collect();
            let s = sender.clone();
            add_instrument_btn.connect_clicked(move |_| {
                let dialog = adw::AlertDialog::new(Some("Add Instrument"), None);
                dialog.add_response("cancel", "Cancel");
                dialog.add_response("add", "Add");
                dialog.set_default_response(Some("add"));
                dialog.set_response_appearance("add", adw::ResponseAppearance::Suggested);

                let prefs = adw::PreferencesGroup::new();

                let name_row = adw::EntryRow::builder().title("Name").build();

                let type_strs: Vec<&str> = default_instrument_types().to_vec();
                let type_model = gtk::StringList::new(&type_strs);
                let type_row = adw::ComboRow::builder().title("Type").build();
                type_row.set_model(Some(&type_model));

                let strings_row = adw::SpinRow::with_range(1.0, 20.0, 1.0);
                strings_row.set_title("Strings");
                strings_row.set_value(6.0);

                let frets_row = adw::SpinRow::with_range(1.0, 48.0, 1.0);
                frets_row.set_title("Frets");
                frets_row.set_value(24.0);

                let temp_strs: Vec<&str> = temperament_names.iter().map(|t| t.as_str()).collect();
                let temp_model = gtk::StringList::new(&temp_strs);
                let temp_row = adw::ComboRow::builder().title("Temperament").build();
                temp_row.set_model(Some(&temp_model));

                // Auto-update string count when instrument type changes.
                {
                    let strings_row2 = strings_row.clone();
                    type_row.connect_selected_notify(move |row| {
                        let types = default_instrument_types();
                        if let Some(t) = types.get(row.selected() as usize) {
                            strings_row2.set_value(default_string_count(t) as f64);
                        }
                    });
                }

                prefs.add(&name_row);
                prefs.add(&type_row);
                prefs.add(&temp_row);
                prefs.add(&strings_row);
                prefs.add(&frets_row);
                dialog.set_extra_child(Some(&prefs));

                let s2 = s.clone();
                let temp_names2 = temperament_names.clone();
                dialog.connect_response(None, move |_, response| {
                    if response != "add" {
                        return;
                    }
                    let types = default_instrument_types();
                    let type_idx = type_row.selected() as usize;
                    let temp_idx = temp_row.selected() as usize;
                    s2.input(AppMsg::AddInstrumentConfirmed(Instrument {
                        name: name_row.text().to_string(),
                        instrument_type: types
                            .get(type_idx)
                            .copied()
                            .unwrap_or("Other")
                            .to_string(),
                        temperament_name: temp_names2
                            .get(temp_idx)
                            .cloned()
                            .unwrap_or_default(),
                        num_strings: strings_row.value() as u32,
                        num_frets: frets_row.value() as u32,
                        fret_markers: vec![],
                        left_handed: None,
                    }));
                });

                dialog.present(Some(&root_ref));
            });
        }

        // ── Wire dropdown signals ─────────────────────────────────────────

        let instrument_sig = instrument_dd.connect_selected_notify(glib::clone!(
            #[strong]
            sender,
            move |dd| {
                sender.input(AppMsg::InstrumentSelected(dd.selected()));
            }
        ));
        Self::repopulate(
            &instrument_dd,
            &instrument_sig,
            &model.instrument_names,
            model.state.selected_instrument_idx.unwrap_or(0),
        );

        let sc_sig = sc_dd.connect_selected_notify(glib::clone!(
            #[strong]
            sender,
            move |dd| {
                sender.input(AppMsg::ScaleOrChordSelected(dd.selected()));
            }
        ));
        let sc_sel = match model.state.diagram_settings.mode {
            DiagramMode::Scale => model.state.selected_scale_idx,
            DiagramMode::Chord => model.state.selected_chord_idx,
        };
        Self::repopulate(&sc_dd, &sc_sig, &model.scale_names, sc_sel);

        let tuning_sig = tuning_dd.connect_selected_notify(glib::clone!(
            #[strong]
            sender,
            move |dd| {
                sender.input(AppMsg::TuningSelected(dd.selected()));
            }
        ));
        Self::repopulate(
            &tuning_dd,
            &tuning_sig,
            &model.tuning_names,
            model.state.selected_tuning_idx,
        );

        // ── Wire SpinButton signals ───────────────────────────────────────

        let edo = model
            .state
            .current_temperament()
            .map(|t| t.divisions as f64)
            .unwrap_or(12.0);
        key_spin.set_range(0.0, (edo - 1.0).max(0.0));
        key_spin.set_increments(1.0, 4.0);
        key_spin.set_value(model.state.diagram_settings.key as f64);
        key_spin.connect_value_changed(glib::clone!(
            #[strong]
            sender,
            move |s| {
                sender.input(AppMsg::KeyChanged(s.value()));
            }
        ));

        fret_spin.set_range(0.0, 24.0);
        fret_spin.set_increments(1.0, 4.0);
        fret_spin.set_value(model.state.diagram_settings.fret_offset as f64);
        fret_spin.connect_value_changed(glib::clone!(
            #[strong]
            sender,
            move |s| {
                sender.input(AppMsg::FretOffsetChanged(s.value()));
            }
        ));

        // ── Initial diagram render (background thread) ───────────────────

        {
            let s = sender.clone();
            crate::render::submit_render(model.state.clone(), vec![], move |data, w, h, layout| {
                s.input(AppMsg::DiagramReady(data, w, h, layout));
            });
        }

        let widgets = AppWidgets {
            window: root.clone(),
            instrument_dd,
            instrument_sig,
            sc_label,
            sc_dd,
            sc_sig,
            tuning_dd,
            tuning_sig,
            key_spin,
            fret_spin,
            diagram,
            mic_btn,
            mic_sig,
        };

        ComponentParts { model, widgets }
    }

    fn update(&mut self, msg: AppMsg, sender: ComponentSender<Self>) {
        match msg {
            AppMsg::DiagramReady(data, w, h, layout) => {
                *self.pending_texture.borrow_mut() = Some((data, w, h));
                self.diagram_layout = Some(layout);
                return;
            }
            AppMsg::DiagramClicked { x, y, widget_w, widget_h } => {
                let Some(layout) = &self.diagram_layout else { return };
                // The Picture uses ContentFit::Contain. The rasterized image is
                // always 800px wide; use the stored texture dimensions.
                let tex = self.pending_texture.borrow();
                let Some((_, img_w, img_h)) = tex.as_ref() else { return };
                let img_w = *img_w as f64;
                let img_h = *img_h as f64;
                // Compute the displayed rect within the widget (letterbox).
                let scale = (widget_w / img_w).min(widget_h / img_h);
                let disp_w = img_w * scale;
                let disp_h = img_h * scale;
                let off_x = (widget_w - disp_w) / 2.0;
                let off_y = (widget_h - disp_h) / 2.0;
                let elem_x = x - off_x;
                let elem_y = y - off_y;
                let Some(hit) = hit_test(layout, disp_w, disp_h, elem_x, elem_y) else { return };

                let step = hit.absolute_step;
                self.flash_steps.push(step);
                self.needs_render.set(true);

                // Play audio for the tapped note.
                if let Some(backend) = &self.audio {
                    use xen_sequencer::backend::AudioBackend;
                    if let (Some(temp), Some(tuning)) = (
                        self.state.current_temperament().cloned(),
                        self.state.current_tuning().cloned(),
                    ) {
                        let prefs = &self.state.preferences;
                        let root_hz = tuning.step0_hz(
                            prefs.concert_hz,
                            prefs.concert_octave,
                            temp.divisions,
                            (*temp.period.numer(), *temp.period.denom()),
                        );
                        let freq = xen_theory::theory::edo_step_to_hz(
                            hit.absolute_step,
                            temp.divisions,
                            (*temp.period.numer(), *temp.period.denom()),
                            root_hz,
                        );
                        backend.play_chord(vec![freq], Box::new(|| {}));
                    }
                }

                // Schedule expiry after 180 ms on the GTK main thread.
                glib::timeout_add_local_once(
                    std::time::Duration::from_millis(180),
                    glib::clone!(
                        #[strong]
                        sender,
                        move || { sender.input(AppMsg::DiagramFlashExpired(step)); }
                    ),
                );
                // Trigger a re-render immediately to show the flash.
            }
            AppMsg::DiagramFlashExpired(step) => {
                if let Some(pos) = self.flash_steps.iter().position(|&s| s == step) {
                    self.flash_steps.remove(pos);
                }
                self.needs_render.set(true);
                return;
            }
            AppMsg::InstrumentSelected(i) => {
                if (i as usize) < self.state.instruments.len() {
                    self.state.select_instrument(i as usize);
                    self.rebuild_derived();
                }
            }
            AppMsg::ScaleOrChordSelected(i) => match self.state.diagram_settings.mode {
                DiagramMode::Scale => self.state.selected_scale_idx = i as usize,
                DiagramMode::Chord => self.state.selected_chord_idx = i as usize,
            },
            AppMsg::TuningSelected(i) => {
                self.state.selected_tuning_idx = i as usize;
            }
            AppMsg::ModeSet(mode) => {
                self.state.diagram_settings.mode = mode;
            }
            AppMsg::KeyChanged(v) => {
                self.state.diagram_settings.key = v as u32;
            }
            AppMsg::FretOffsetChanged(v) => {
                self.state.diagram_settings.fret_offset = v as u32;
            }
            AppMsg::AddInstrumentConfirmed(instrument) => {
                self.state.instruments.push(instrument);
                self.state.selected_instrument_idx =
                    Some(self.state.instruments.len() - 1);
                self.rebuild_derived();
            }
            AppMsg::MicToggle => {
                if let Some(h) = self.mic_handle.take() {
                    // Already listening — stop.
                    h.stop();
                    self.mic_steps.clear();
                    self.needs_render.set(true);
                    return;
                }
                // Start listening.
                if let Some(h) = xen_sequencer::native_mic::NativeMicHandle::spawn(4096) {
                    let handle = std::sync::Arc::new(h);
                    self.mic_handle = Some(handle.clone());
                    let sender_worker = sender.clone();
                    let state_snapshot = self.state.clone();
                    let detector_kind = state_snapshot.preferences.pitch_detector.clone();
                    std::thread::Builder::new()
                        .name("xen-mic-detect".into())
                        .spawn(move || {
                            use xen_dsp::detector::PitchDetector;
                            use app_common::preferences::PitchDetectorKind;
                            let mut detector: Box<dyn PitchDetector> = match detector_kind {
                                PitchDetectorKind::Yin => Box::new(
                                    xen_dsp::yin::YinDetector::new(0.15, 50.0, 4000.0),
                                ),
                                PitchDetectorKind::IterF0 => Box::new(
                                    xen_dsp::iterf0::IterF0Detector::new(6, 8, 70.0, 2000.0, 0.15),
                                ),
                            };
                            let mut holdoff: std::collections::HashMap<i32, u8> =
                                Default::default();
                            const HOLDOFF_FRAMES: u8 = 12;
                            while let Some(ev) = handle.next_event_blocking() {
                                match ev {
                                    xen_sequencer::mic::MicEvent::Samples { rate, data } => {
                                        let pitches = detector.detect(&data, rate);
                                        let fresh =
                                            app_common::pitch_map::detected_to_absolute_steps(
                                                &state_snapshot,
                                                &pitches,
                                            );
                                        holdoff.retain(|_, c| {
                                            if *c == 0 { false } else { *c -= 1; true }
                                        });
                                        for s in fresh {
                                            holdoff.insert(s, HOLDOFF_FRAMES);
                                        }
                                        sender_worker.input(AppMsg::MicPitchesChanged(
                                            holdoff.keys().copied().collect(),
                                        ));
                                    }
                                    xen_sequencer::mic::MicEvent::Stopped
                                    | xen_sequencer::mic::MicEvent::Error(_) => {
                                        sender_worker.input(AppMsg::MicStopped);
                                        break;
                                    }
                                    xen_sequencer::mic::MicEvent::Started { .. } => {}
                                }
                            }
                        })
                        .ok();
                }
                return;
            }
            AppMsg::MicPitchesChanged(steps) => {
                self.mic_steps = steps;
                self.needs_render.set(true);
                return;
            }
            AppMsg::MicStopped => {
                self.mic_handle = None;
                self.mic_steps.clear();
                self.needs_render.set(true);
                return;
            }
        }
        storage::save(&self.state);
        self.needs_render.set(true);
        self.needs_ui_sync.set(true);
    }

    fn update_view(&self, widgets: &mut Self::Widgets, sender: ComponentSender<Self>) {
        // Only repopulate dropdowns and sync spinners when instrument/scale/tuning/key
        // state has actually changed.  Skipping this for mic/flash/render-only updates
        // prevents set_model from closing open popups and stealing focus.
        if self.needs_ui_sync.get() {
            self.needs_ui_sync.set(false);

            Self::repopulate(
                &widgets.instrument_dd,
                &widgets.instrument_sig,
                &self.instrument_names,
                self.state.selected_instrument_idx.unwrap_or(0),
            );

            widgets
                .sc_label
                .set_label(match self.state.diagram_settings.mode {
                    DiagramMode::Scale => "Scale",
                    DiagramMode::Chord => "Chord",
                });

            let (sc_names, sc_sel) = match self.state.diagram_settings.mode {
                DiagramMode::Scale => (&self.scale_names, self.state.selected_scale_idx),
                DiagramMode::Chord => (&self.chord_names, self.state.selected_chord_idx),
            };
            Self::repopulate(&widgets.sc_dd, &widgets.sc_sig, sc_names, sc_sel);

            Self::repopulate(
                &widgets.tuning_dd,
                &widgets.tuning_sig,
                &self.tuning_names,
                self.state.selected_tuning_idx,
            );

            let edo = self
                .state
                .current_temperament()
                .map(|t| t.divisions as f64)
                .unwrap_or(12.0);
            let cur_key = self.state.diagram_settings.key as f64;
            widgets.key_spin.set_range(0.0, (edo - 1.0).max(0.0));
            if (widgets.key_spin.value() - cur_key).abs() > 0.5 {
                widgets.key_spin.set_value(cur_key);
            }

            let cur_fret = self.state.diagram_settings.fret_offset as f64;
            if (widgets.fret_spin.value() - cur_fret).abs() > 0.5 {
                widgets.fret_spin.set_value(cur_fret);
            }
        }

        // Sync mic button active state (in case of external stop via error/device loss).
        // Block the toggled signal while changing active state so we don't re-enter MicToggle.
        let listening = self.mic_handle.is_some();
        if widgets.mic_btn.is_active() != listening {
            widgets.mic_btn.block_signal(&widgets.mic_sig);
            widgets.mic_btn.set_active(listening);
            widgets.mic_btn.unblock_signal(&widgets.mic_sig);
        }

        // Spawn a background render when state has changed
        if self.needs_render.get() {
            self.needs_render.set(false);
            let s = sender.clone();
            let mut flash = self.flash_steps.clone();
            flash.extend(self.mic_steps.iter().copied());
            crate::render::submit_render(self.state.clone(), flash, move |data, w, h, layout| {
                s.input(AppMsg::DiagramReady(data, w, h, layout));
            });
        }

        // Apply completed render — take() so set_paintable is only called when
        // fresh data arrives, never on repeated update_view calls with stale data.
        if let Some((data, w, h)) = self.pending_texture.borrow_mut().take() {
            let tex = crate::render::bytes_to_texture(data, w, h);
            widgets.diagram.set_paintable(Some(&tex));
        }
    }
}
