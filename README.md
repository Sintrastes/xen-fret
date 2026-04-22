# Xen Fret

<p align="center">
  <img src="https://github.com/Sintrastes/xen-fret/blob/master/app/dioxus_frontend/assets/icons/icon-256.png?raw=true">
</p>

<p align="center">
  <a href="https://haskell.org/">
    <img src="https://img.shields.io/badge/Language-Rust-orange">
  </a>
  <a href="https://github.com/Sintrastes/xen-fret/actions/workflows/deploy.yml">
    <img src="https://github.com/Sintrastes/xen-fret/actions/workflows/deploy.yml/badge.svg">
  </a>
</p>

Xen Fret is a appliation for players of microtonal (a.k.a. xenharmonic) fretted string instruments. It works similarly to other interactive "scale practice" applications for guitar such as [fretastic](https://fretastic.com/guitar), but for microtonal instruments.

Xen Fret uses data from the [xen wiki](https://en.xen.wiki/w/Main_Page), together with some of my own manually entered data for my own 22-EDO guitar. This application is developed in my free time, and will always be free and open source. If you see anything wrong / missing, or have a feature request, please either [file an issue](https://github.com/Sintrastes/xen-fret/issues/new) or submit a PR! 

*Note*: The project's logo is a diagram showing the 8-edo subset of 16-edo, generated with a step size of 7. The outer edges of the diagram represent edo steps, while the inner edges represent the generator chain. This particular scale was chosen over other [MOS](https://en.xen.wiki/w/List_of_MOS_scales_in_16edo) scales because I thought this one looked the best as a logo. Mathematically, the logo is a type of [star polygon](https://en.wikipedia.org/wiki/Star_polygon).

Getting started
---------------

The latest version of Xen Fret is currently hosted [here](https://sintrastes.github.io/xen_fret/) as a static page on github pages. Builds for other platforms (desktop / mobile versions) can be found in the CI artifacts, and will be included in future releases.

Features
--------

  - Manage scales and chords for different instruments. When you first open the app, you'll start by entering the exact configuration of your instrument, even if it's something exotic like a [Kite guitar](https://kiteguitar.com/).
  - Practice scales / chords.
  - Export or print fretboard diagrams in svg and pdf format.
  - Customize the theme of the application and of the fretboard diagrams.
  - Generate microtonal backing tracks / jam tracks to practice soloing. 
  - Real-time pitch detection / transcription for microtonal instruments. (WIP)
  - Chord / scale detection. (WIP)
  - Native clients for Web, Android, and Linux (GTK). Currently other platforms use just a web app deployed with Tauri.

Screenshots
-----------

**Web**:

<img width="1018" height="758" alt="Xen Fret Web" src="https://github.com/user-attachments/assets/e2a33ff6-0a96-495d-9379-5af12edb77aa" />

**GTK**:

<img width="1016" height="812" alt="Xen Fret GTK" src="https://github.com/user-attachments/assets/7c971bc2-59eb-4228-8a6f-5bbcce833b0a" />

Building
--------

Xen Fret is a [Dioxus](https://dioxuslabs.com/) web application compiled to WebAssembly. If you're new to Dioxus, start with the [Dioxus getting started guide](https://dioxuslabs.com/learn/0.7/getting_started) — the steps below cover only what's specific to this project.

**Prerequisites**

1. **Rust (nightly)** — the project pins to the nightly toolchain via `rust-toolchain.toml`, so any recent nightly will be selected automatically once you have `rustup` installed. The WASM target is also declared in that file and will be added automatically.

2. **Dioxus CLI** — install version 0.7.x to match the crate:
   ```
   cargo install dioxus-cli --version "^0.7"
   ```

**Development server**

```
dx serve
```

This compiles to WASM and opens the app in your browser with hot-reload on changes to `src/` and `assets/`.

**Production build**

```
dx build --release --platform web
```

Output lands in `dist/`. The site is fully static and can be deployed to any static host (GitHub Pages, Netlify, etc.).

**GitHub Pages build**

The app is served from a subdirectory (`/xen-fret/`), which requires a different base path. Use `Dioxus.gh-pages.toml` for that build:

```
cp Dioxus.gh-pages.toml Dioxus.toml && dx build --release --platform web
```

Nerd Stuff
----------

Xen Fret has been a longstanding project of mine. Originally it was a simple [CGI](https://en.wikipedia.org/wiki/Common_Gateway_Interface) binary that could be served up in Apache and generated static SVG images built in Haskell using [diagrams](https://diagrams.github.io/). Later on I turned it into a simple web app using [reflex](https://reflex-frp.org/). Since then I've re-written the whole thing in Rust with [Dioxus](https://dioxuslabs.com/) and the assistance of agentic AI.

I am a bit sad with the sunsetting of the old Haskell version, but I think ultimately Rust is the right choice for performance ("Rust -> WASM" is much faster than "Haskell -> JS" was in the browser) and ecosystem, allowing us to iterate faster and deliver more features to the Xenharmonic community. 🎸
