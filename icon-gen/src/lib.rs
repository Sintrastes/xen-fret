use hagoromo::*;
use kurbo::Point;
use std::f64::consts::PI;
use std::path::Path;

// ── Palette ───────────────────────────────────────────────────────────────────

const POLYGON_THICKNESS: f64 = 3.5;
const MOS_THICKNESS: f64 = 5.0;
const INK: Color = Color::rgb_bytes(255, 255, 255);
static ACCENT: Color = Color::rgb_bytes(145, 188, 255);

const BG_R: u8 = 15;
const BG_G: u8 = 37;
const BG_B: u8 = 68;

const CORNER_RADIUS_FRAC: f32 = 0.22;
const SQUARE_SCALE: f32 = 0.82;
const DIAGRAM_SCALE: f32 = 0.82;

// ── Config ────────────────────────────────────────────────────────────────────

pub struct Config {
    pub edo: usize,
    pub generator: usize,
    pub num_notes: usize,
    pub radius: f64,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            edo: 16,
            generator: 6,
            num_notes: 8,
            radius: 50.0,
        }
    }
}

// ── Geometry ──────────────────────────────────────────────────────────────────

fn ngon_vertices(n: usize, radius: f64) -> Vec<Point> {
    (0..n)
        .map(|i| {
            let angle = -PI / 2.0 + i as f64 * 2.0 * PI / n as f64;
            Point::new(radius * angle.cos(), radius * angle.sin())
        })
        .collect()
}

fn mos_indices(edo: usize, generator: usize, num_notes: usize) -> Vec<usize> {
    (0..num_notes).map(|i| (i * generator) % edo).collect()
}

// ── Diagram builder ───────────────────────────────────────────────────────────

fn build_diagram(cfg: &Config) -> Diagram {
    let verts = ngon_vertices(cfg.edo, cfg.radius);
    let mos_idx = mos_indices(cfg.edo, cfg.generator, cfg.num_notes);

    let mut d = Diagram::empty();

    let ngon_outline: Vec<Point> = verts
        .iter()
        .cloned()
        .chain(std::iter::once(verts[0]))
        .collect();
    d = d + polyline(&ngon_outline)
        .lc(INK)
        .lw(POLYGON_THICKNESS)
        .opacity(1.0);

    let mut scale_path: Vec<Point> = mos_idx.iter().map(|&i| verts[i]).collect();
    scale_path.push(scale_path[0]);
    d = d + polyline(&scale_path).lc(ACCENT).lw(MOS_THICKNESS);

    for (i, v) in verts.iter().enumerate() {
        if !mos_idx.contains(&i) {
            d = d + circle(cfg.radius * 0.07)
                .fc(INK)
                .lw(0.0)
                .translate(v.x, v.y);
        }
    }
    let dot_r = cfg.radius * 0.13;
    for &idx in &mos_idx {
        let v = verts[idx];
        d = d + circle(dot_r).fc(ACCENT).lw(0.0).translate(v.x, v.y);
    }

    d
}

// ── SVG ───────────────────────────────────────────────────────────────────────

pub fn render_icon_svg(cfg: &Config) -> String {
    let diagram = build_diagram(cfg);
    render_svg(
        &diagram,
        &RenderOptions {
            padding: 0.0,
            background: None,
            default_stroke_width: hagoromo::THIN,
        },
    )
}

// ── Rasterization ─────────────────────────────────────────────────────────────

pub fn svg_to_pixmap(svg: &str, size: u32) -> tiny_skia::Pixmap {
    use tiny_skia::*;

    let mut pixmap = Pixmap::new(size, size).expect("zero size");
    let s = size as f32;
    let sq = s * SQUARE_SCALE;
    let offset = (s - sq) / 2.0;
    let r = sq * CORNER_RADIUS_FRAC;
    let k = r * 0.5523;
    let x0 = offset;
    let x1 = offset + sq;
    let y0 = offset;
    let y1 = offset + sq;

    // Background gradient
    let bg_paint = {
        let mut p = Paint::default();
        let stops = vec![
            GradientStop::new(0.0, Color::from_rgba8(30, 58, 100, 255)),
            GradientStop::new(1.0, Color::from_rgba8(5, 18, 40, 255)),
        ];
        p.shader = LinearGradient::new(
            Point::from_xy(x0, y0),
            Point::from_xy(x1, y1),
            stops,
            SpreadMode::Pad,
            Transform::identity(),
        )
        .unwrap_or(Shader::SolidColor(Color::from_rgba8(BG_R, BG_G, BG_B, 255)));
        p.anti_alias = true;
        p
    };

    let mut pb = PathBuilder::new();
    pb.move_to(x0 + r, y0);
    pb.line_to(x1 - r, y0);
    pb.cubic_to(x1 - r + k, y0, x1, y0 + r - k, x1, y0 + r);
    pb.line_to(x1, y1 - r);
    pb.cubic_to(x1, y1 - r + k, x1 - r + k, y1, x1 - r, y1);
    pb.line_to(x0 + r, y1);
    pb.cubic_to(x0 + r - k, y1, x0, y1 - r + k, x0, y1 - r);
    pb.line_to(x0, y0 + r);
    pb.cubic_to(x0, y0 + r - k, x0 + r - k, y0, x0 + r, y0);
    pb.close();
    pixmap.fill_path(
        &pb.finish().unwrap(),
        &bg_paint,
        FillRule::Winding,
        Transform::identity(),
        None,
    );

    // Render SVG diagram
    let options = usvg::Options::default();
    let tree = usvg::Tree::from_str(svg, &options).expect("invalid SVG");
    let diagram_size = sq * DIAGRAM_SCALE;
    let svg_w = tree.size().width();
    let svg_h = tree.size().height();
    let scale = diagram_size / svg_w.max(svg_h);
    let scaled_w = (svg_w * scale) as u32;
    let scaled_h = (svg_h * scale) as u32;

    let mut diagram_pixmap = Pixmap::new(scaled_w.max(1), scaled_h.max(1)).expect("zero size");
    resvg::render(
        &tree,
        Transform::from_scale(scale, scale),
        &mut diagram_pixmap.as_mut(),
    );

    // Radial gradient modulate on diagram lines
    let dw = diagram_pixmap.width() as f32;
    let dh = diagram_pixmap.height() as f32;
    let dcx = dw / 2.0;
    let dcy = dh / 2.0;
    let grad_r = dw.max(dh) * 0.55;
    let grad_stops = vec![
        GradientStop::new(0.0, Color::from_rgba8(130, 150, 220, 255)),
        GradientStop::new(0.5, Color::from_rgba8(220, 230, 255, 255)),
        GradientStop::new(1.0, Color::from_rgba8(255, 255, 255, 255)),
    ];
    if let Some(shader) = RadialGradient::new(
        Point::from_xy(dcx, dcy),
        Point::from_xy(dcx, dcy),
        grad_r,
        grad_stops,
        SpreadMode::Pad,
        Transform::identity(),
    ) {
        let mut grad_paint = Paint::default();
        grad_paint.shader = shader;
        grad_paint.blend_mode = BlendMode::Modulate;
        diagram_pixmap.fill_rect(
            Rect::from_xywh(0.0, 0.0, dw, dh).unwrap(),
            &grad_paint,
            Transform::identity(),
            None,
        );
    }

    let cx = ((s - scaled_w as f32) / 2.0) as i32;
    let cy = ((s - scaled_h as f32) / 2.0) as i32;
    pixmap.draw_pixmap(
        cx,
        cy,
        diagram_pixmap.as_ref(),
        &PixmapPaint::default(),
        Transform::identity(),
        None,
    );

    pixmap
}

/// Like `svg_to_pixmap` but the background fills the entire canvas with no
/// transparent border and no rounded corners. Used for Android where the
/// system applies the circle crop itself.
pub fn svg_to_pixmap_fullbleed(svg: &str, size: u32) -> tiny_skia::Pixmap {
    use tiny_skia::*;

    let mut pixmap = Pixmap::new(size, size).expect("zero size");
    let s = size as f32;

    // Background gradient spans the full canvas
    let bg_paint = {
        let mut p = Paint::default();
        let stops = vec![
            GradientStop::new(0.0, Color::from_rgba8(30, 58, 100, 255)),
            GradientStop::new(1.0, Color::from_rgba8(5, 18, 40, 255)),
        ];
        p.shader = LinearGradient::new(
            Point::from_xy(0.0, 0.0),
            Point::from_xy(s, s),
            stops,
            SpreadMode::Pad,
            Transform::identity(),
        )
        .unwrap_or(Shader::SolidColor(Color::from_rgba8(BG_R, BG_G, BG_B, 255)));
        p.anti_alias = true;
        p
    };
    pixmap.fill_rect(
        Rect::from_xywh(0.0, 0.0, s, s).unwrap(),
        &bg_paint,
        Transform::identity(),
        None,
    );

    // Render SVG diagram — use DIAGRAM_SCALE of full canvas
    let options = usvg::Options::default();
    let tree = usvg::Tree::from_str(svg, &options).expect("invalid SVG");
    let diagram_size = s * DIAGRAM_SCALE;
    let svg_w = tree.size().width();
    let svg_h = tree.size().height();
    let scale = diagram_size / svg_w.max(svg_h);
    let scaled_w = (svg_w * scale) as u32;
    let scaled_h = (svg_h * scale) as u32;

    let mut diagram_pixmap = Pixmap::new(scaled_w.max(1), scaled_h.max(1)).expect("zero size");
    resvg::render(&tree, Transform::from_scale(scale, scale), &mut diagram_pixmap.as_mut());

    // Radial gradient modulate on diagram lines
    let dw = diagram_pixmap.width() as f32;
    let dh = diagram_pixmap.height() as f32;
    let dcx = dw / 2.0;
    let dcy = dh / 2.0;
    let grad_r = dw.max(dh) * 0.55;
    let grad_stops = vec![
        GradientStop::new(0.0, Color::from_rgba8(130, 150, 220, 255)),
        GradientStop::new(0.5, Color::from_rgba8(220, 230, 255, 255)),
        GradientStop::new(1.0, Color::from_rgba8(255, 255, 255, 255)),
    ];
    if let Some(shader) = RadialGradient::new(
        Point::from_xy(dcx, dcy),
        Point::from_xy(dcx, dcy),
        grad_r,
        grad_stops,
        SpreadMode::Pad,
        Transform::identity(),
    ) {
        let mut grad_paint = Paint::default();
        grad_paint.shader = shader;
        grad_paint.blend_mode = BlendMode::Modulate;
        diagram_pixmap.fill_rect(
            Rect::from_xywh(0.0, 0.0, dw, dh).unwrap(),
            &grad_paint,
            Transform::identity(),
            None,
        );
    }

    let cx = ((s - scaled_w as f32) / 2.0) as i32;
    let cy = ((s - scaled_h as f32) / 2.0) as i32;
    pixmap.draw_pixmap(cx, cy, diagram_pixmap.as_ref(), &PixmapPaint::default(), Transform::identity(), None);

    pixmap
}

/// Background layer for Android adaptive icon — gradient fill, no diagram, full canvas.
/// Gradient spans the same proportional area as the macOS rounded-square version so
/// the colours match when Android composites the two layers.
pub fn svg_to_background_pixmap(size: u32) -> tiny_skia::Pixmap {
    use tiny_skia::*;

    let mut pixmap = Pixmap::new(size, size).expect("zero size");
    let s = size as f32;
    // Match macOS: gradient corners are at the rounded-square inset, not the full canvas edge.
    let sq = s * SQUARE_SCALE;
    let offset = (s - sq) / 2.0;
    let x0 = offset;
    let x1 = offset + sq;
    let y0 = offset;
    let y1 = offset + sq;

    let mut p = Paint::default();
    let stops = vec![
        GradientStop::new(0.0, Color::from_rgba8(30, 58, 100, 255)),
        GradientStop::new(1.0, Color::from_rgba8(5, 18, 40, 255)),
    ];
    p.shader = LinearGradient::new(
        tiny_skia::Point::from_xy(x0, y0),
        tiny_skia::Point::from_xy(x1, y1),
        stops,
        SpreadMode::Pad,
        Transform::identity(),
    )
    .unwrap_or(Shader::SolidColor(Color::from_rgba8(BG_R, BG_G, BG_B, 255)));
    p.anti_alias = true;
    pixmap.fill_rect(Rect::from_xywh(0.0, 0.0, s, s).unwrap(), &p, Transform::identity(), None);

    pixmap
}

/// Foreground layer for Android adaptive icon — diagram on transparent background.
/// Diagram is scaled to ~66% of the canvas so it stays within the adaptive icon safe zone.
pub fn svg_to_foreground_pixmap(svg: &str, size: u32) -> tiny_skia::Pixmap {
    use tiny_skia::*;

    let mut pixmap = Pixmap::new(size, size).expect("zero size");
    let s = size as f32;

    let options = usvg::Options::default();
    let tree = usvg::Tree::from_str(svg, &options).expect("invalid SVG");
    // 0.43: diagram fills ~65% of the visible 72dp circle (108dp canvas),
    // giving ~17% padding on each side — matches macOS rounded-square proportions.
    let diagram_size = s * 0.43;
    let svg_w = tree.size().width();
    let svg_h = tree.size().height();
    let scale = diagram_size / svg_w.max(svg_h);
    let scaled_w = (svg_w * scale) as u32;
    let scaled_h = (svg_h * scale) as u32;

    let mut diagram_pixmap = Pixmap::new(scaled_w.max(1), scaled_h.max(1)).expect("zero size");
    resvg::render(&tree, Transform::from_scale(scale, scale), &mut diagram_pixmap.as_mut());

    // Radial gradient modulate on diagram lines
    let dw = diagram_pixmap.width() as f32;
    let dh = diagram_pixmap.height() as f32;
    let dcx = dw / 2.0;
    let dcy = dh / 2.0;
    let grad_r = dw.max(dh) * 0.55;
    let grad_stops = vec![
        GradientStop::new(0.0, Color::from_rgba8(130, 150, 220, 255)),
        GradientStop::new(0.5, Color::from_rgba8(220, 230, 255, 255)),
        GradientStop::new(1.0, Color::from_rgba8(255, 255, 255, 255)),
    ];
    if let Some(shader) = RadialGradient::new(
        tiny_skia::Point::from_xy(dcx, dcy),
        tiny_skia::Point::from_xy(dcx, dcy),
        grad_r,
        grad_stops,
        SpreadMode::Pad,
        Transform::identity(),
    ) {
        let mut grad_paint = Paint::default();
        grad_paint.shader = shader;
        grad_paint.blend_mode = BlendMode::Modulate;
        diagram_pixmap.fill_rect(
            Rect::from_xywh(0.0, 0.0, dw, dh).unwrap(),
            &grad_paint,
            Transform::identity(),
            None,
        );
    }

    let cx = ((s - scaled_w as f32) / 2.0) as i32;
    let cy = ((s - scaled_h as f32) / 2.0) as i32;
    pixmap.draw_pixmap(cx, cy, diagram_pixmap.as_ref(), &PixmapPaint::default(), Transform::identity(), None);

    pixmap
}

pub fn encode_png(pixmap: &tiny_skia::Pixmap) -> Vec<u8> {
    pixmap.encode_png().expect("PNG encoding failed")
}

pub fn encode_webp(pixmap: &tiny_skia::Pixmap) -> Vec<u8> {
    let mut buf = Vec::new();
    image::codecs::webp::WebPEncoder::new_lossless(&mut buf)
        .encode(
            pixmap.data(),
            pixmap.width(),
            pixmap.height(),
            image::ExtendedColorType::Rgba8,
        )
        .expect("WebP encoding failed");
    buf
}

pub fn write_file(path: &Path, data: &[u8]) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("failed to create directory");
        let stem = path.file_stem().unwrap_or_default();
        for ext in ["webp", "png", "jpg"] {
            let competing = parent.join(stem).with_extension(ext);
            if competing != path && competing.exists() {
                let _ = std::fs::remove_file(&competing);
            }
        }
    }
    std::fs::write(path, data).expect("failed to write file");
    eprintln!("  wrote {}", path.display());
}

// ── High-level helpers ────────────────────────────────────────────────────────

/// Write SVG + PNGs for the given config. Returns the SVG string.
pub fn generate_assets(cfg: &Config, svg_path: &Path, png_dir: &Path) -> String {
    let svg = render_icon_svg(cfg);
    std::fs::write(svg_path, &svg).expect("failed to write SVG");
    for size in [16u32, 32, 64, 128, 256, 512, 1024] {
        let png = encode_png(&svg_to_pixmap(&svg, size));
        write_file(&png_dir.join(format!("icon-{size}.png")), &png);
    }
    svg
}

/// Build icon.icns from the PNGs in png_dir using macOS iconutil.
/// Silently skips on non-macOS hosts.
pub fn build_icns(png_dir: &Path, icns_path: &Path) {
    let iconset = png_dir.join("Icon.iconset");
    let _ = std::fs::create_dir_all(&iconset);

    let copies: &[(&str, &str)] = &[
        ("icon-16.png", "icon_16x16.png"),
        ("icon-32.png", "icon_16x16@2x.png"),
        ("icon-32.png", "icon_32x32.png"),
        ("icon-64.png", "icon_32x32@2x.png"),
        ("icon-128.png", "icon_128x128.png"),
        ("icon-256.png", "icon_128x128@2x.png"),
        ("icon-256.png", "icon_256x256.png"),
        ("icon-512.png", "icon_256x256@2x.png"),
        ("icon-512.png", "icon_512x512.png"),
        ("icon-1024.png", "icon_512x512@2x.png"),
    ];
    for (src, dst) in copies {
        let _ = std::fs::copy(png_dir.join(src), iconset.join(dst));
    }

    let status = std::process::Command::new("iconutil")
        .args(["-c", "icns"])
        .arg(&iconset)
        .arg("-o")
        .arg(icns_path)
        .status();

    let _ = std::fs::remove_dir_all(&iconset);

    if let Err(e) = status {
        eprintln!("iconutil failed (non-macOS host?): {e}");
    }
}
