use relm4::gtk::gdk;
use tiny_skia::Pixmap;
use std::sync::{mpsc, OnceLock};

static BRAVURA_BYTES: &[u8] =
    include_bytes!("../../../app/dioxus_frontend/assets/Bravura.woff");

// A closure the persistent render thread executes with pre-built Options.
type RenderTask = Box<dyn FnOnce(&usvg::Options) + Send + 'static>;

fn render_tx() -> &'static mpsc::SyncSender<RenderTask> {
    static TX: OnceLock<mpsc::SyncSender<RenderTask>> = OnceLock::new();
    TX.get_or_init(|| {
        let (tx, rx) = mpsc::sync_channel::<RenderTask>(8);
        std::thread::spawn(move || {
            // Build the Options (including full fontdb) exactly once.
            let mut opts = usvg::Options::default();
            opts.fontdb_mut().load_system_fonts();
            if let Some(sfnt) = app_common::font::woff1_to_sfnt(BRAVURA_BYTES) {
                opts.fontdb_mut().load_font_data(sfnt);
            }
            for task in rx {
                task(&opts);
            }
        });
        tx
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
/// Calls `callback` with the pixel data when done.
pub fn submit_render(
    state: app_common::state::AppState,
    callback: impl FnOnce(Vec<u8>, u32, u32) + Send + 'static,
) {
    let tx = render_tx();
    let _ = tx.send(Box::new(move |opts| {
        if let Some(svg) = app_common::diagrams::build_svg(&state, "") {
            if let Some((data, w, h)) = rasterize(&svg, opts) {
                callback(data, w, h);
            }
        }
    }));
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
