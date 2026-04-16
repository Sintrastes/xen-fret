use clap::Parser;
use icon_gen::{Config, build_icns, encode_webp, generate_assets, render_icon_svg, svg_to_pixmap, write_file};
use std::path::Path;

#[derive(Parser)]
#[command(about = "Generate a MOS diagram SVG icon for xen-fret")]
struct Args {
    #[arg(long, default_value_t = Config::default().edo)]
    edo: usize,
    #[arg(long, default_value_t = Config::default().generator)]
    generator: usize,
    #[arg(long, default_value_t = Config::default().num_notes)]
    num_notes: usize,
    #[arg(long, default_value_t = Config::default().radius)]
    radius: f64,
    #[arg(long, short)]
    output: Option<String>,
    #[arg(long)]
    android_res: Option<String>,
    #[arg(long)]
    png_dir: Option<String>,
}

fn main() {
    let args = Args::parse();
    let cfg = Config { edo: args.edo, generator: args.generator, num_notes: args.num_notes, radius: args.radius };
    let svg = render_icon_svg(&cfg);

    match &args.output {
        Some(path) => std::fs::write(path, &svg).expect("failed to write SVG"),
        None if args.android_res.is_none() && args.png_dir.is_none() => print!("{}", svg),
        None => {}
    }

    if let Some(res_dir) = &args.android_res {
        let buckets = [
            ("mipmap-mdpi", 48u32), ("mipmap-hdpi", 72), ("mipmap-xhdpi", 96),
            ("mipmap-xxhdpi", 144), ("mipmap-xxxhdpi", 192),
        ];
        for (bucket, size) in &buckets {
            let webp = encode_webp(&svg_to_pixmap(&svg, *size));
            write_file(&Path::new(res_dir).join(bucket).join("ic_launcher.webp"), &webp);
        }
    }

    if let Some(dir) = &args.png_dir {
        let dir = Path::new(dir);
        for size in [16u32, 32, 64, 128, 256, 512, 1024] {
            let png = icon_gen::encode_png(&svg_to_pixmap(&svg, size));
            write_file(&dir.join(format!("icon-{size}.png")), &png);
        }
    }
}
