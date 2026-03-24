use dioxus::desktop::tao::dpi::LogicalPosition;
use dioxus::desktop::tao::platform::macos::WindowBuilderExtMacOS;
use dioxus::desktop::{Config, WindowBuilder};

/// Configure the macOS window for a native look: transparent titlebar with
/// traffic lights inset into the sidebar, content extending behind the titlebar.
pub fn configure_window(wb: WindowBuilder) -> WindowBuilder {
    wb.with_titlebar_transparent(true)
        .with_title_hidden(true)
        .with_fullsize_content_view(true)
        .with_traffic_light_inset(LogicalPosition::new(16.0, 18.0))
}

/// Apply macOS-specific desktop configuration: transparent webview background
/// and platform CSS overrides injected via a custom `<style>` block.
pub fn configure_desktop(cfg: Config) -> Config {
    cfg.with_background_color((0, 0, 0, 0))
        .with_custom_head(MACOS_CSS.to_string())
}

/// With fullsize content view the content extends behind the titlebar,
/// so no height offset is needed when computing inner size.
pub const TITLEBAR_HEIGHT_OFFSET: u32 = 0;

// ── macOS CSS overrides ──────────────────────────────────────────────────────
//
// Injected via with_custom_head so it loads after the main stylesheet and
// overrides by cascade order. Keeps assets/style.css platform-agnostic.

const MACOS_CSS: &str = r##"<style>
/* ================================================================
   macOS Platform Overrides
   ================================================================ */

/* ── Typography ─────────────────────────────────────────────────── */

body {
    font-family: -apple-system, BlinkMacSystemFont, 'SF Pro Text', 'SF Pro Display', system-ui, sans-serif !important;
    font-size: 13px;
    -webkit-font-smoothing: antialiased;
}

.page-title {
    font-size: 20px;
    font-weight: 600;
    letter-spacing: -0.01em;
}

.page-subtitle {
    font-size: 13px;
}

.card-heading {
    font-size: 13px;
    font-weight: 600;
}

.form-label {
    font-size: 12px;
    font-weight: 500;
    color: var(--text-muted);
}

/* ── Colors: macOS system accent ────────────────────────────────── */

:root {
    --primary:        #007aff;
    --primary-hover:  #0062cc;
    --primary-light:  rgba(0, 122, 255, 0.10);
    --primary-text:   #ffffff;

    --bg:             #f5f5f7;
    --surface:        #ffffff;
    --surface-raised: #f9f9fb;

    --border:         rgba(0, 0, 0, 0.12);
    --border-focus:   #007aff;

    --shadow-sm:      0 0.5px 1px rgba(0, 0, 0, 0.04);
    --shadow:         0 1px 2px rgba(0, 0, 0, 0.06);
    --shadow-md:      0 2px 8px rgba(0, 0, 0, 0.08);
    --shadow-lg:      0 4px 16px rgba(0, 0, 0, 0.10);

    --radius-sm:      5px;
    --radius:         8px;
    --radius-lg:      12px;
}

[data-theme="dark"] {
    --primary:        #0a84ff;
    --primary-hover:  #409cff;
    --primary-light:  rgba(10, 132, 255, 0.12);

    --bg:             #1e1e1e;
    --surface:        #2a2a2a;
    --surface-raised: #333333;

    --border:         rgba(255, 255, 255, 0.10);
    --border-focus:   #0a84ff;

    --shadow-sm:      0 0.5px 1px rgba(0, 0, 0, 0.2);
    --shadow:         0 1px 2px rgba(0, 0, 0, 0.25);
    --shadow-md:      0 2px 8px rgba(0, 0, 0, 0.3);
    --shadow-lg:      0 4px 16px rgba(0, 0, 0, 0.35);
}

/* ── Sidebar ────────────────────────────────────────────────────── */

.sidebar {
    position: relative;
    padding-top: 52px;
    background: rgba(236, 236, 240, 0.80) !important;
    -webkit-backdrop-filter: blur(20px);
    backdrop-filter: blur(20px);
    border-right: 0.5px solid rgba(0, 0, 0, 0.12);
}

[data-theme="dark"] .sidebar {
    background: rgba(30, 30, 30, 0.75) !important;
    border-right-color: rgba(255, 255, 255, 0.08);
}

/* ── Titlebar drag regions ───────────────────────────────────────── */
/* Real DOM elements that call dioxus::desktop::window().drag() on
   mousedown. Styled as transparent overlays in the titlebar zone. */

.titlebar-drag-region {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 52px;
    z-index: 100;
    cursor: default;
}

.sidebar-drag {
    z-index: 50; /* below sidebar nav links but above sidebar background */
}

.main-drag {
    z-index: 100;
    border-bottom: 0.5px solid var(--border);
    background: rgba(246, 246, 248, 0.80);
    -webkit-backdrop-filter: blur(20px);
    backdrop-filter: blur(20px);
}

[data-theme="dark"] .main-drag {
    background: rgba(40, 40, 40, 0.80);
}

.sidebar-nav {
    padding: 8px 8px !important;
    gap: 1px !important;
    position: relative;
    z-index: 60; /* above sidebar drag region */
}

.sidebar-link {
    padding: 6px 10px !important;
    border-radius: 5px !important;
    font-size: 13px !important;
    font-weight: 400 !important;
    color: rgba(0, 0, 0, 0.70) !important;
}

.sidebar-link:hover {
    background: rgba(0, 0, 0, 0.04) !important;
    color: rgba(0, 0, 0, 0.85) !important;
}

.sidebar-link.active {
    background: rgba(0, 0, 0, 0.08) !important;
    color: #007aff !important;
    font-weight: 500 !important;
}

[data-theme="dark"] .sidebar-link {
    color: rgba(255, 255, 255, 0.70) !important;
}

[data-theme="dark"] .sidebar-link:hover {
    background: rgba(255, 255, 255, 0.06) !important;
    color: rgba(255, 255, 255, 0.90) !important;
}

[data-theme="dark"] .sidebar-link.active {
    background: rgba(255, 255, 255, 0.10) !important;
    color: #0a84ff !important;
}

.nav-icon {
    font-size: 14px;
    width: 18px;
}

.sidebar-footer {
    padding: 8px !important;
    border-top: 0.5px solid rgba(0, 0, 0, 0.10) !important;
    position: relative;
    z-index: 60; /* above sidebar drag region */
}

[data-theme="dark"] .sidebar-footer {
    border-top-color: rgba(255, 255, 255, 0.08) !important;
}

.preferences-link {
    color: rgba(0, 0, 0, 0.55) !important;
    font-size: 13px;
}

[data-theme="dark"] .preferences-link {
    color: rgba(255, 255, 255, 0.55) !important;
}

/* Sidebar logo (hidden on desktop via cfg!, but style in case it appears) */
.sidebar-logo .logo-text {
    color: #1d1d1f;
}

[data-theme="dark"] .sidebar-logo .logo-text {
    color: #e5e5e7;
}

/* ── Main content: unified titlebar toolbar ─────────────────────── */
/* A full-width toolbar strip across the top of main-content,
   spanning both the controls panel and preview panel on the Home
   page, just like a native macOS unified toolbar. */

.main-content {
    border-left: none; /* sidebar border-right handles it */
    position: relative;
}

/* ── Home page: toolbar integration ─────────────────────────────── */

/* Both panels need top padding to clear the toolbar */
.controls-panel {
    padding-top: 52px;
}

.preview-panel {
    padding-top: 52px;
    position: relative;
}

/* Remove the preview-header bar — lift the action buttons into
   the unified toolbar region instead. */
.preview-header {
    position: absolute;
    top: 0;
    right: 0;
    z-index: 150;  /* above the main-drag region */
    height: 52px;
    width: auto;
    padding: 0 16px !important;
    border: none !important;
    border-bottom: none !important;
    background: transparent !important;
    box-shadow: none !important;
    display: flex;
    align-items: center;
    justify-content: flex-end;
}

/* Hide the title/subtitle — context is already in the controls panel */
.preview-header-info {
    display: none;
}

/* ── Toolbar icon buttons ───────────────────────────────────────── */
/* macOS Notes/Finder-style toolbar icons: simple gray glyphs with
   no background. Hover shows a subtle rounded-rect highlight. */

.btn-play,
.btn-mic {
    width: 30px !important;
    height: 24px !important;
    border-radius: 5px !important;
    border: none !important;
    background: transparent !important;
    color: var(--text-muted) !important;
    font-size: 12px !important;
    transition: background 120ms ease, color 120ms ease;
    display: inline-flex;
    align-items: center;
    justify-content: center;
}

.btn-play:hover,
.btn-mic:hover {
    background: rgba(0, 0, 0, 0.08) !important;
    color: var(--text) !important;
}

[data-theme="dark"] .btn-play:hover,
[data-theme="dark"] .btn-mic:hover {
    background: rgba(255, 255, 255, 0.12) !important;
    color: var(--text) !important;
}

.btn-play.playing {
    color: #ff3b30 !important;
    background: transparent !important;
    border: none !important;
}

.btn-play.playing:hover {
    background: rgba(255, 59, 48, 0.10) !important;
}

.btn-mic.listening {
    color: #ff3b30 !important;
    background: transparent !important;
    border: none !important;
    animation: none !important;
}

.btn-mic.listening:hover {
    background: rgba(255, 59, 48, 0.10) !important;
}

/* Toolbar SVG icons inherit color */
.toolbar-icon {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    line-height: 0;
}

.toolbar-icon svg {
    display: block;
}

/* Octave selector: minimal toolbar style */
.btn-octave {
    width: 22px;
    height: 22px;
    border-radius: 5px;
    border: none !important;
    background: transparent;
    color: var(--text-muted);
    font-size: 12px;
    font-weight: 500;
    transition: background 120ms ease, color 120ms ease;
}

.btn-octave:hover {
    background: rgba(0, 0, 0, 0.08) !important;
    color: var(--text);
}

[data-theme="dark"] .btn-octave:hover {
    background: rgba(255, 255, 255, 0.12) !important;
}

.octave-label {
    font-size: 11px !important;
    font-weight: 500;
    color: var(--text-muted);
    min-width: 32px;
    text-align: center;
}

/* ── Non-home pages: page header in titlebar zone ───────────────── */

.page-header {
    position: relative;
    z-index: 150; /* above main-drag */
}

/* ── Segmented controls ─────────────────────────────────────────── */

.segmented-control {
    background: rgba(0, 0, 0, 0.06);
    border: none;
    border-radius: 7px;
    padding: 2px;
    gap: 1px;
}

[data-theme="dark"] .segmented-control {
    background: rgba(255, 255, 255, 0.08);
}

.seg-btn {
    padding: 4px 12px;
    border-radius: 6px;
    font-size: 12px;
    font-weight: 500;
    color: var(--text-muted);
}

.seg-btn.active {
    background: var(--surface);
    color: var(--text);
    box-shadow: 0 0.5px 2px rgba(0, 0, 0, 0.12), 0 0.5px 0.5px rgba(0, 0, 0, 0.08);
}

[data-theme="dark"] .seg-btn.active {
    background: rgba(255, 255, 255, 0.14);
    box-shadow: 0 0.5px 2px rgba(0, 0, 0, 0.3);
}

.seg-btn:not(.active):hover {
    color: var(--text);
}

/* ── Form inputs ────────────────────────────────────────────────── */

.form-input,
.form-select {
    padding: 6px 10px;
    border: 0.5px solid var(--border);
    border-radius: 5px;
    font-size: 13px;
}

.form-input:focus,
.form-select:focus {
    border-color: var(--border-focus);
    box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.25);
}

[data-theme="dark"] .form-input:focus,
[data-theme="dark"] .form-select:focus {
    box-shadow: 0 0 0 3px rgba(10, 132, 255, 0.25);
}

/* ── Dropdowns & Combobox (modern macOS NSPopUpButton style) ──── */
/* Both use the same visual language: rounded pill trigger with
   up/down chevron, white popover with rounded-corner highlighted
   items and a checkmark on the selected row. */

/* -- Shared trigger style ---------------------------------------- */

.dropdown-trigger,
.cmb-field {
    padding: 4px 8px !important;
    border: 1px solid #b0b0b0 !important;
    border-radius: 6px !important;
    font-size: 13px !important;
    background: #fff !important;
    box-shadow: 0 0.5px 1px rgba(0, 0, 0, 0.10) !important;
    min-height: 24px !important;
}

[data-theme="dark"] .dropdown-trigger,
[data-theme="dark"] .cmb-field {
    background: #2c2c2e !important;
    border-color: rgba(255, 255, 255, 0.25) !important;
    box-shadow: 0 0.5px 1.5px rgba(0, 0, 0, 0.4), 0 0 0 0.5px rgba(255, 255, 255, 0.06) !important;
}

.dropdown-trigger:focus,
.dropdown-trigger--open,
.cmb-field--open,
.cmb-field:focus-within {
    border-color: #007aff !important;
    box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.25) !important;
}

[data-theme="dark"] .dropdown-trigger:focus,
[data-theme="dark"] .dropdown-trigger--open,
[data-theme="dark"] .cmb-field--open,
[data-theme="dark"] .cmb-field:focus-within {
    border-color: #0a84ff !important;
    box-shadow: 0 0 0 3px rgba(10, 132, 255, 0.25) !important;
}

/* -- Dropdown chevron: replace single triangle with up/down pair -- */

.dropdown-chevron {
    width: 10px !important;
    height: auto !important;
    display: flex !important;
    flex-direction: column !important;
    align-items: center !important;
    justify-content: center !important;
    gap: 2px !important;
    flex-shrink: 0 !important;
    overflow: visible !important;
}

.dropdown-chevron svg {
    display: none !important;
}

/* Up triangle */
.dropdown-chevron::before {
    content: "" !important;
    display: block !important;
    width: 0 !important;
    height: 0 !important;
    border-left: 3.5px solid transparent !important;
    border-right: 3.5px solid transparent !important;
    border-bottom: 4px solid var(--text-muted) !important;
}

/* Down triangle */
.dropdown-chevron::after {
    content: "" !important;
    display: block !important;
    width: 0 !important;
    height: 0 !important;
    border-left: 3.5px solid transparent !important;
    border-right: 3.5px solid transparent !important;
    border-top: 4px solid var(--text-muted) !important;
}

.dropdown-chevron--open {
    transform: none !important;
}

/* -- Combobox input specifics ------------------------------------ */

.cmb-field {
    gap: 4px !important;
    overflow: visible !important;
}

.cmb-input {
    font-size: 13px !important;
    padding: 2px 0 !important;
}

/* Combobox toggle: match dropdown chevron style */
.cmb-toggle {
    padding: 0 4px !important;
    background: transparent !important;
    border-radius: 0 !important;
    display: flex !important;
    flex-direction: column !important;
    align-items: center !important;
    justify-content: center !important;
    gap: 2px !important;
    width: 18px !important;
    overflow: visible !important;
    flex-shrink: 0 !important;
}

/* Hide the existing SVG inside toggle */
.cmb-toggle svg {
    display: none !important;
}

/* Up triangle */
.cmb-toggle::before {
    content: "" !important;
    display: block !important;
    width: 0 !important;
    height: 0 !important;
    border-left: 3.5px solid transparent !important;
    border-right: 3.5px solid transparent !important;
    border-bottom: 4px solid var(--text-muted) !important;
}

/* Down triangle */
.cmb-toggle::after {
    content: "" !important;
    display: block !important;
    width: 0 !important;
    height: 0 !important;
    border-left: 3.5px solid transparent !important;
    border-right: 3.5px solid transparent !important;
    border-top: 4px solid var(--text-muted) !important;
}

.cmb-toggle:hover {
    background: transparent !important;
}

.cmb-toggle:hover::before {
    border-bottom-color: var(--text) !important;
}

.cmb-toggle:hover::after {
    border-top-color: var(--text) !important;
}

[data-theme="dark"] .cmb-toggle {
    background: transparent !important;
}

[data-theme="dark"] .cmb-toggle:hover {
    background: transparent !important;
}

/* -- Shared popover style ---------------------------------------- */

.dropdown-content,
.cmb-list {
    border: 0.5px solid rgba(0, 0, 0, 0.12) !important;
    border-radius: 8px !important;
    box-shadow: 0 5px 24px rgba(0, 0, 0, 0.14), 0 0 0 0.5px rgba(0, 0, 0, 0.06) !important;
    padding: 4px !important;
    background: #fff !important;
}

[data-theme="dark"] .dropdown-content,
[data-theme="dark"] .cmb-list {
    background: #2c2c2e !important;
    border-color: rgba(255, 255, 255, 0.14) !important;
    box-shadow: 0 5px 24px rgba(0, 0, 0, 0.5), 0 0 0 0.5px rgba(255, 255, 255, 0.08) !important;
}

/* -- Shared item style ------------------------------------------- */

.dropdown-item,
.cmb-item {
    padding: 4px 8px !important;
    border-radius: 5px !important;
    font-size: 13px !important;
    margin: 0 !important;
}

.dropdown-item:hover,
.dropdown-item--focused,
.cmb-item:hover,
.cmb-item--hi {
    background: var(--primary) !important;
    color: #fff !important;
}

.dropdown-item--selected,
.cmb-item--selected {
    font-weight: 500 !important;
}

/* When not hovered, selected item text is normal (checkmark shows selection) */
.dropdown-item--selected:not(:hover):not(.dropdown-item--focused),
.cmb-item--selected:not(:hover):not(.cmb-item--hi) {
    color: var(--text) !important;
}

.dropdown-item:hover.dropdown-item--selected,
.dropdown-item--focused.dropdown-item--selected,
.cmb-item:hover.cmb-item--selected,
.cmb-item--hi.cmb-item--selected {
    color: #fff !important;
}

.dropdown-item:hover .dropdown-item-check,
.dropdown-item--focused .dropdown-item-check {
    color: #fff !important;
}

.cmb-check {
    color: var(--primary) !important;
}

.cmb-item:hover .cmb-check,
.cmb-item--hi .cmb-check {
    color: #fff !important;
}

.cmb-no-match {
    font-size: 12px !important;
    color: var(--text-muted) !important;
    padding: 8px 10px !important;
}

/* ── Buttons ────────────────────────────────────────────────────── */

.btn {
    padding: 5px 12px;
    border-radius: 5px;
    font-size: 13px;
    font-weight: 500;
}

.btn:active {
    transform: none;
    filter: brightness(0.92);
}

.btn-primary {
    background: linear-gradient(180deg, #1a8cff 0%, #007aff 100%);
}

.btn-primary:hover {
    background: linear-gradient(180deg, #0072dd 0%, #0062cc 100%);
}

[data-theme="dark"] .btn-primary {
    background: linear-gradient(180deg, #2e90ff 0%, #0a84ff 100%);
}

[data-theme="dark"] .btn-primary:hover {
    background: linear-gradient(180deg, #409cff 0%, #2e90ff 100%);
}

.btn-outline {
    border: 0.5px solid var(--border);
}

/* ── Toggle switch ──────────────────────────────────────────────── */

.toggle-slider {
    width: 36px;
    height: 20px;
    border-radius: 10px;
}

.toggle-slider::after {
    width: 14px;
    height: 14px;
    top: 3px;
    left: 3px;
}

.toggle input:checked + .toggle-slider {
    background: #34c759;  /* macOS system green */
}

[data-theme="dark"] .toggle input:checked + .toggle-slider {
    background: #30d158;  /* macOS dark mode green */
}

.toggle input:checked + .toggle-slider::after {
    transform: translateX(16px);
}

/* ── Radio buttons ──────────────────────────────────────────────── */

input[type="radio"] {
    accent-color: #007aff;
}

[data-theme="dark"] input[type="radio"] {
    accent-color: #0a84ff;
}

/* ── Checkboxes ─────────────────────────────────────────────────── */

.form-check-input {
    accent-color: #007aff;
}

[data-theme="dark"] .form-check-input {
    accent-color: #0a84ff;
}

/* ── Key picker buttons ─────────────────────────────────────────── */

.key-btn {
    border-radius: 5px;
    border: 0.5px solid var(--border);
    font-size: 13px;
}

.key-btn--active {
    background: #007aff;
    border-color: #007aff;
}

[data-theme="dark"] .key-btn--active {
    background: #0a84ff;
    border-color: #0a84ff;
}

/* ── Cards → macOS grouped sections ────────────────────────────── */
/* Native macOS apps (System Preferences, Finder inspector) use flat
   grouped sections with hairline separators — no shadows or heavy
   rounded containers. */

.card {
    border: none !important;
    border-radius: 0 !important;
    box-shadow: none !important;
    background: transparent !important;
    border-bottom: 0.5px solid var(--border) !important;
    padding: 16px 0 !important;
}

.card:last-child {
    border-bottom: none !important;
}

.card-heading {
    border-bottom: none !important;
    padding-bottom: 8px !important;
    margin-bottom: 12px !important;
    font-size: 13px !important;
    font-weight: 600 !important;
    color: var(--text) !important;
    text-transform: none !important;
}

/* Preferences page: keep grouped card look (like System Settings) */
.prefs-card {
    background: var(--surface) !important;
    border: 0.5px solid var(--border) !important;
    border-radius: 10px !important;
    padding: 16px !important;
    border-bottom: 0.5px solid var(--border) !important;
}

/* Group cards on list pages (Temperaments, Scales, etc.) */
.group-card {
    background: var(--surface) !important;
    border: 0.5px solid var(--border) !important;
    border-radius: 10px !important;
    padding: 12px 16px !important;
    border-bottom: 0.5px solid var(--border) !important;
}

/* ── Controls & preview panels (Home page) ─────────────────────── */

.controls-panel {
    background: var(--bg) !important;
    border-left: 0.5px solid var(--border) !important;
}

.preview-panel {
    background: var(--bg) !important;
}

/* Fretboard diagram: flat canvas, no card chrome */
.fretboard-card {
    background: transparent !important;
    border: none !important;
    border-radius: 0 !important;
    box-shadow: none !important;
    padding: 20px !important;
}

/* ── Color picker ───────────────────────────────────────────────── */

.color-swatch {
    border-radius: 4px;
    border: 0.5px solid var(--border);
}

/* ── Scrollbars (native overlay style) ──────────────────────────── */

::-webkit-scrollbar {
    width: 8px;
    height: 8px;
}

::-webkit-scrollbar-track {
    background: transparent;
}

::-webkit-scrollbar-thumb {
    background: rgba(0, 0, 0, 0.2);
    border-radius: 4px;
    border: 2px solid transparent;
    background-clip: content-box;
}

[data-theme="dark"] ::-webkit-scrollbar-thumb {
    background: rgba(255, 255, 255, 0.2);
    border: 2px solid transparent;
    background-clip: content-box;
}

::-webkit-scrollbar-thumb:hover {
    background: rgba(0, 0, 0, 0.35);
    background-clip: content-box;
}

[data-theme="dark"] ::-webkit-scrollbar-thumb:hover {
    background: rgba(255, 255, 255, 0.35);
    background-clip: content-box;
}

/* ── Page layout (slightly tighter) ─────────────────────────────── */

.page {
    padding: 52px 28px 24px 28px;
    gap: 20px;
}

</style>"##;
