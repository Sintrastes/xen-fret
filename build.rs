/// Build script: generate icon assets and inject them into platform bundles.
///
/// 1. Always: regenerate SVG + PNGs + icns from icon-gen's Config::default().
/// 2. Android: inject webp mipmaps and remove adaptive-icon XML.
/// 3. macOS: copy icon.icns into the app bundle Resources directory.
fn main() {
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();

    // Always re-run on every build.

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    let manifest  = std::path::Path::new(&manifest_dir);
    let target_dx = manifest.join("target").join("dx");
    let svg_path  = manifest.join("assets").join("xen-fret-icon.svg");
    let png_dir   = manifest.join("assets").join("icons");
    let icns_path = png_dir.join("icon.icns");

    // ── 1. Regenerate SVG + PNGs + icns ──────────────────────────────────────
    let cfg = icon_gen::Config::default();
    icon_gen::generate_assets(&cfg, &svg_path, &png_dir);
    icon_gen::build_icns(&png_dir, &icns_path);

    // ── 2/3. Platform injection ───────────────────────────────────────────────
    match target_os.as_str() {
        "android" => inject_android(&target_dx),
        "macos"   => inject_macos_icns(&target_dx),
        _ => {}
    }
}

// ── Android ───────────────────────────────────────────────────────────────────

fn inject_android(target_dx: &std::path::Path) {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    let src_res = std::path::Path::new(&manifest_dir).join("assets").join("android").join("res");
    if !src_res.is_dir() { return; }

    let res_dirs = find_android_res_dirs(target_dx);
    if res_dirs.is_empty() { return; }

    for res_dir in &res_dirs {
        // Remove stale .webp launcher icons left by dx so they don't clash with our .png files.
        let mipmap_prefixes = ["mipmap-mdpi", "mipmap-hdpi", "mipmap-xhdpi", "mipmap-xxhdpi", "mipmap-xxxhdpi"];
        let launcher_stems  = ["ic_launcher", "ic_launcher_background", "ic_launcher_foreground", "ic_launcher_monochrome"];
        for prefix in &mipmap_prefixes {
            for stem in &launcher_stems {
                let _ = std::fs::remove_file(res_dir.join(prefix).join(format!("{stem}.webp")));
            }
        }
        copy_dir_recursive(&src_res, res_dir);
    }
}

/// Recursively copy all files from `src` into `dst`, skipping `.DS_Store`.
fn copy_dir_recursive(src: &std::path::Path, dst: &std::path::Path) {
    let Ok(entries) = std::fs::read_dir(src) else { return };
    for entry in entries.flatten() {
        let name = entry.file_name();
        if name == ".DS_Store" { continue; }
        let src_path = entry.path();
        let dst_path = dst.join(&name);
        if src_path.is_dir() {
            let _ = std::fs::create_dir_all(&dst_path);
            copy_dir_recursive(&src_path, &dst_path);
        } else {
            if let Some(parent) = dst_path.parent() {
                let _ = std::fs::create_dir_all(parent);
            }
            if let Err(e) = std::fs::copy(&src_path, &dst_path) {
                eprintln!("cargo:warning=icon build.rs: failed to copy {}: {e}", src_path.display());
            }
        }
    }
}

fn find_android_res_dirs(target_dx: &std::path::Path) -> Vec<std::path::PathBuf> {
    let mut found = Vec::new();
    let Ok(apps) = std::fs::read_dir(target_dx) else { return found };
    for app in apps.flatten() {
        let Ok(profiles) = std::fs::read_dir(app.path()) else { continue };
        for profile in profiles.flatten() {
            let res = profile.path()
                .join("android").join("app").join("app")
                .join("src").join("main").join("res");
            if res.is_dir() { found.push(res); }
        }
    }
    found
}

// ── macOS ─────────────────────────────────────────────────────────────────────

fn inject_macos_icns(target_dx: &std::path::Path) {
    let src = std::path::Path::new("assets/icons/icon.icns");
    if !src.exists() { return; }

    let Ok(apps) = std::fs::read_dir(target_dx) else { return };
    for app in apps.flatten() {
        let Ok(profiles) = std::fs::read_dir(app.path()) else { continue };
        for profile in profiles.flatten() {
            let macos_dir = profile.path().join("macos");
            let Ok(bundles) = std::fs::read_dir(&macos_dir) else { continue };
            for bundle in bundles.flatten() {
                let resources = bundle.path().join("Contents").join("Resources");
                if resources.is_dir() {
                    let dst = resources.join("icon.icns");
                    if let Err(e) = std::fs::copy(src, &dst) {
                        eprintln!("cargo:warning=icon build.rs: failed to copy icns: {e}");
                    }
                }
            }
        }
    }
}

