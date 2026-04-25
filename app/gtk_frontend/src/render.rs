use fretboard_diagrams::DiagramLayout;
use relm4::gtk::gdk;
use tiny_skia::Pixmap;
use std::sync::{Arc, Condvar, Mutex, OnceLock};

static BRAVURA_BYTES: &[u8] =
    include_bytes!("../../../app/dioxus_frontend/assets/Bravura.woff");

// A closure the persistent render thread executes with pre-built Options.
type RenderTask = Box<dyn FnOnce(&usvg::Options) + Send + 'static>;

// Last-write-wins slot: the caller always overwrites whatever was pending,
// so a state-change render immediately displaces any queued mic render.
fn render_slot() -> &'static Arc<(Mutex<Option<RenderTask>>, Condvar)> {
    static SLOT: OnceLock<Arc<(Mutex<Option<RenderTask>>, Condvar)>> = OnceLock::new();
    SLOT.get_or_init(|| {
        let slot: Arc<(Mutex<Option<RenderTask>>, Condvar)> =
            Arc::new((Mutex::new(None), Condvar::new()));
        let slot2 = slot.clone();
        std::thread::spawn(move || {
            let mut opts = usvg::Options::default();
            opts.fontdb_mut().load_system_fonts();
            if let Some(sfnt) = app_common::font::woff1_to_sfnt(BRAVURA_BYTES) {
                opts.fontdb_mut().load_font_data(sfnt);
            }
            loop {
                let task = {
                    let (lock, cvar) = &*slot2;
                    let mut guard = lock.lock().unwrap();
                    while guard.is_none() {
                        guard = cvar.wait(guard).unwrap();
                    }
                    guard.take().unwrap()
                };
                task(&opts);
            }
        });
        slot
    })
}

fn set_concrete_size<'a>(svg: &'a str, target_w: f32) -> std::borrow::Cow<'a, str> {
    if !svg.contains(r#"width="100%""#) {
        return svg.into();
    }
    if let Some(start) = svg.find(r#"viewBox=""#) {
        let rest = &svg[start + 9..];
        if let Some(end) = rest.find('"') {
            let parts: Vec<f32> = rest[..end].split_ascii_whitespace()
                .filter_map(|s| s.parse().ok())
                .collect();
            if parts.len() == 4 && parts[2] > 0.0 {
                let target_h = target_w * parts[3] / parts[2];
                return svg.replacen(
                    r#"width="100%""#,
                    &format!(r#"width="{target_w}" height="{target_h}""#),
                    1,
                ).into();
            }
        }
    }
    svg.into()
}

fn rewrite_font_families(svg: &str) -> String {
    svg.replace(r#"font-family="Bravura, sans-serif""#,
                r#"font-family="Bravura Text, sans-serif""#)
       .replace(r#"font-family='Bravura, sans-serif'"#,
                r#"font-family='Bravura Text, sans-serif'"#)
       .replace(r#"font-family="Bravura""#, r#"font-family="Bravura Text""#)
       .replace(r#"font-family='Bravura'"#, r#"font-family='Bravura Text'"#)
}

fn rasterize(svg: &str, opts: &usvg::Options) -> Option<(Vec<u8>, u32, u32)> {
    let svg = set_concrete_size(svg, 800.0);
    let svg = rewrite_font_families(&svg);
    let tree = usvg::Tree::from_str(&svg, opts).ok()?;
    let w = tree.size().width().ceil() as u32;
    let h = tree.size().height().ceil() as u32;
    if w == 0 || h == 0 { return None; }
    let mut pixmap = Pixmap::new(w, h)?;
    resvg::render(&tree, tiny_skia::Transform::identity(), &mut pixmap.as_mut());
    Some((pixmap.data().to_vec(), w, h))
}

/// Submit a state snapshot for rendering on the persistent render thread.
/// Calls `callback` with the pixel data and diagram layout when done.
/// `playing_steps` drives red highlighting (e.g. click flash).
pub fn submit_render(
    state: app_common::state::AppState,
    playing_steps: Vec<i32>,
    callback: impl FnOnce(Vec<u8>, u32, u32, DiagramLayout) + Send + 'static,
) {
    let (lock, cvar) = &**render_slot();
    *lock.lock().unwrap() = Some(Box::new(move |opts| {
        if let Some((svg, layout)) =
            app_common::diagrams::build_svg_with_layout(&state, "", &[], &playing_steps)
        {
            if let Some((data, w, h)) = rasterize(&svg, opts) {
                callback(data, w, h, layout);
            }
        }
    }));
    cvar.notify_one();
}

/// Wrap raw pixel bytes in a GDK MemoryTexture. Must be called on the GTK main thread.
pub fn bytes_to_texture(data: Vec<u8>, w: u32, h: u32) -> gdk::MemoryTexture {
    let bytes = relm4::gtk::glib::Bytes::from_owned(data);
    gdk::MemoryTexture::new(
        w as i32, h as i32,
        gdk::MemoryFormat::R8g8b8a8Premultiplied,
        &bytes,
        w as usize * 4,
    )
}
