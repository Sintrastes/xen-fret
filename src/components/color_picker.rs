use dioxus::prelude::*;

use crate::models::Color;

// ── HSL ↔ RGB conversion helpers ─────────────────────────────────────────────

fn rgb_to_hsl(r: u8, g: u8, b: u8) -> (f64, f64, f64) {
    let r = r as f64 / 255.0;
    let g = g as f64 / 255.0;
    let b = b as f64 / 255.0;
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let l = (max + min) / 2.0;
    if (max - min).abs() < 1e-10 {
        return (0.0, 0.0, l * 100.0);
    }
    let d = max - min;
    let s = if l > 0.5 { d / (2.0 - max - min) } else { d / (max + min) };
    let h = if (max - r).abs() < 1e-10 {
        ((g - b) / d + if g < b { 6.0 } else { 0.0 }) / 6.0
    } else if (max - g).abs() < 1e-10 {
        ((b - r) / d + 2.0) / 6.0
    } else {
        ((r - g) / d + 4.0) / 6.0
    };
    (h * 360.0, s * 100.0, l * 100.0)
}

fn hue_channel(p: f64, q: f64, mut t: f64) -> f64 {
    if t < 0.0 { t += 1.0; }
    if t > 1.0 { t -= 1.0; }
    if t < 1.0 / 6.0 { return p + (q - p) * 6.0 * t; }
    if t < 0.5 { return q; }
    if t < 2.0 / 3.0 { return p + (q - p) * (2.0 / 3.0 - t) * 6.0; }
    p
}

fn hsl_to_rgb(h: f64, s: f64, l: f64) -> Color {
    let h = h / 360.0;
    let s = s / 100.0;
    let l = l / 100.0;
    if s < 1e-10 {
        let v = (l * 255.0).round() as u8;
        return Color { r: v, g: v, b: v };
    }
    let q = if l < 0.5 { l * (1.0 + s) } else { l + s - l * s };
    let p = 2.0 * l - q;
    Color {
        r: (hue_channel(p, q, h + 1.0 / 3.0) * 255.0).round() as u8,
        g: (hue_channel(p, q, h) * 255.0).round() as u8,
        b: (hue_channel(p, q, h - 1.0 / 3.0) * 255.0).round() as u8,
    }
}

fn parse_hex(hex: &str) -> Option<Color> {
    let hex = hex.trim_start_matches('#');
    if hex.len() != 6 { return None; }
    let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
    let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
    let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
    Some(Color { r, g, b })
}

// ── Component ─────────────────────────────────────────────────────────────────

#[component]
pub fn ColorPicker(value: Color, on_change: EventHandler<Color>) -> Element {
    let (ih, is, il) = rgb_to_hsl(value.r, value.g, value.b);
    let mut hue = use_signal(|| ih);
    let mut sat = use_signal(|| is);
    let mut lit = use_signal(|| il);
    let mut hex_text = use_signal(|| value.to_hex().trim_start_matches('#').to_string());
    let mut open = use_signal(|| false);

    let h = *hue.read();
    let s = *sat.read();
    let l = *lit.read();
    let hex = hex_text.read().clone();
    let swatch_hex = hsl_to_rgb(h, s, l).to_hex();

    let sat_gradient = format!(
        "linear-gradient(to right, hsl({h:.0},0%,{l:.0}%), hsl({h:.0},100%,{l:.0}%))"
    );
    let lit_gradient = format!(
        "linear-gradient(to right, hsl({h:.0},{s:.0}%,0%), hsl({h:.0},{s:.0}%,50%), hsl({h:.0},{s:.0}%,100%))"
    );

    rsx! {
        div { class: "cp-wrap",
            button {
                class: "cp-swatch",
                r#type: "button",
                style: "background:{swatch_hex};",
                title: "{swatch_hex}",
                onclick: move |_| {
                    let cur = *open.read();
                    open.set(!cur);
                },
            }

            if *open.read() {
                div {
                    class: "cp-backdrop",
                    onclick: move |_| open.set(false),
                }
                div {
                    class: "cp-popover",
                    onclick: |e| e.stop_propagation(),

                    // Preview
                    div { class: "cp-preview",
                        div { class: "cp-preview-swatch", style: "background:{swatch_hex};" }
                        span { class: "cp-preview-hex", "{swatch_hex}" }
                    }

                    // Hue
                    div { class: "cp-row",
                        label { class: "cp-lbl", "H" }
                        input {
                            class: "cp-slider",
                            r#type: "range",
                            min: "0", max: "360", step: "1",
                            value: "{h:.0}",
                            style: "--track-bg: linear-gradient(to right,#f00,#ff0,#0f0,#0ff,#00f,#f0f,#f00);",
                            oninput: move |e| {
                                if let Ok(v) = e.value().parse::<f64>() {
                                    hue.set(v);
                                    let c = hsl_to_rgb(v, *sat.read(), *lit.read());
                                    hex_text.set(c.to_hex().trim_start_matches('#').to_string());
                                    on_change.call(c);
                                }
                            }
                        }
                        span { class: "cp-val", "{h:.0}°" }
                    }

                    // Saturation
                    div { class: "cp-row",
                        label { class: "cp-lbl", "S" }
                        input {
                            class: "cp-slider",
                            r#type: "range",
                            min: "0", max: "100", step: "1",
                            value: "{s:.0}",
                            style: "--track-bg: {sat_gradient};",
                            oninput: move |e| {
                                if let Ok(v) = e.value().parse::<f64>() {
                                    sat.set(v);
                                    let c = hsl_to_rgb(*hue.read(), v, *lit.read());
                                    hex_text.set(c.to_hex().trim_start_matches('#').to_string());
                                    on_change.call(c);
                                }
                            }
                        }
                        span { class: "cp-val", "{s:.0}%" }
                    }

                    // Lightness
                    div { class: "cp-row",
                        label { class: "cp-lbl", "L" }
                        input {
                            class: "cp-slider",
                            r#type: "range",
                            min: "0", max: "100", step: "1",
                            value: "{l:.0}",
                            style: "--track-bg: {lit_gradient};",
                            oninput: move |e| {
                                if let Ok(v) = e.value().parse::<f64>() {
                                    lit.set(v);
                                    let c = hsl_to_rgb(*hue.read(), *sat.read(), v);
                                    hex_text.set(c.to_hex().trim_start_matches('#').to_string());
                                    on_change.call(c);
                                }
                            }
                        }
                        span { class: "cp-val", "{l:.0}%" }
                    }

                    // Hex text input
                    div { class: "cp-hex-row",
                        span { class: "cp-hex-prefix", "#" }
                        input {
                            class: "cp-hex-input",
                            r#type: "text",
                            maxlength: "7",
                            value: "{hex}",
                            oninput: move |e| hex_text.set(e.value()),
                            onchange: move |e| {
                                if let Some(c) = parse_hex(&e.value()) {
                                    let (nh, ns, nl) = rgb_to_hsl(c.r, c.g, c.b);
                                    hue.set(nh);
                                    sat.set(ns);
                                    lit.set(nl);
                                    hex_text.set(c.to_hex().trim_start_matches('#').to_string());
                                    on_change.call(c);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
