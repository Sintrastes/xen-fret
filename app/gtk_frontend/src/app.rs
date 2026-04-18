use app_common::{
    state::{AppState, DiagramMode},
    storage,
};
use glib::SignalHandlerId;
use relm4::{
    adw::{self, prelude::*},
    gtk::{self, glib, prelude::*},
    ComponentParts, ComponentSender, SimpleComponent,
};

pub struct AppModel {
    state: AppState,
    instrument_names: Vec<String>,
    scale_names: Vec<String>,
    chord_names: Vec<String>,
    tuning_names: Vec<String>,
    pending_texture: Option<(Vec<u8>, u32, u32)>,
    needs_render: std::cell::Cell<bool>,
}

#[derive(Debug)]
pub enum AppMsg {
    InstrumentSelected(u32),
    ScaleOrChordSelected(u32),
    TuningSelected(u32),
    ModeSet(DiagramMode),
    KeyChanged(f64),
    FretOffsetChanged(f64),
    DiagramReady(Vec<u8>, u32, u32),
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
    instrument_dd: gtk::DropDown,
    #[allow(dead_code)]
    instrument_sig: SignalHandlerId,
    sc_label: gtk::Label,
    sc_dd: gtk::DropDown,
    sc_sig: SignalHandlerId,
    tuning_dd: gtk::DropDown,
    tuning_sig: SignalHandlerId,
    key_spin: gtk::SpinButton,
    fret_spin: gtk::SpinButton,
    diagram: gtk::Picture,
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
        let state = storage::load();
        let mut model = AppModel {
            state,
            instrument_names: vec![],
            scale_names: vec![],
            chord_names: vec![],
            tuning_names: vec![],
            pending_texture: None,
            needs_render: std::cell::Cell::new(false),
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

        // ── Sidebar widgets ───────────────────────────────────────────────

        let instrument_label = gtk::Label::builder()
            .label("Instrument")
            .halign(gtk::Align::Start)
            .build();
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
        sidebar_box.append(&instrument_label);
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
            crate::render::submit_render(model.state.clone(), move |data, w, h| {
                s.input(AppMsg::DiagramReady(data, w, h));
            });
        }

        let widgets = AppWidgets {
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
        };

        ComponentParts { model, widgets }
    }

    fn update(&mut self, msg: AppMsg, _sender: ComponentSender<Self>) {
        match msg {
            AppMsg::DiagramReady(data, w, h) => {
                self.pending_texture = Some((data, w, h));
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
        }
        storage::save(&self.state);
        self.needs_render.set(true);
    }

    fn update_view(&self, widgets: &mut Self::Widgets, sender: ComponentSender<Self>) {
        // Sync mode label
        widgets
            .sc_label
            .set_label(match self.state.diagram_settings.mode {
                DiagramMode::Scale => "Scale",
                DiagramMode::Chord => "Chord",
            });

        // Repopulate scale/chord dropdown
        let (sc_names, sc_sel) = match self.state.diagram_settings.mode {
            DiagramMode::Scale => (&self.scale_names, self.state.selected_scale_idx),
            DiagramMode::Chord => (&self.chord_names, self.state.selected_chord_idx),
        };
        Self::repopulate(&widgets.sc_dd, &widgets.sc_sig, sc_names, sc_sel);

        // Repopulate tuning dropdown
        Self::repopulate(
            &widgets.tuning_dd,
            &widgets.tuning_sig,
            &self.tuning_names,
            self.state.selected_tuning_idx,
        );

        // Sync key SpinButton range (EDO may have changed with instrument)
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

        // Sync fret offset
        let cur_fret = self.state.diagram_settings.fret_offset as f64;
        if (widgets.fret_spin.value() - cur_fret).abs() > 0.5 {
            widgets.fret_spin.set_value(cur_fret);
        }

        // Spawn a background render when state has changed
        if self.needs_render.get() {
            self.needs_render.set(false);
            let s = sender.clone();
            crate::render::submit_render(self.state.clone(), move |data, w, h| {
                s.input(AppMsg::DiagramReady(data, w, h));
            });
        }

        // Apply completed render
        if let Some((data, w, h)) = &self.pending_texture {
            let tex = crate::render::bytes_to_texture(data.clone(), *w, *h);
            widgets.diagram.set_paintable(Some(&tex));
        }
    }
}
