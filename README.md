# Xen Fret

<p align="center">
  <a href="https://haskell.org/">
    <img src="https://img.shields.io/badge/Language-Haskell-blue">
  </a>
  <a href="https://github.com/Sintrastes/xen-fret/actions/workflows/build.yml">
    <img src="https://github.com/Sintrastes/xen-fret/actions/workflows/build.yml/badge.svg">
  </a>
</p>

Xen Fret is a appliation for players of microtonal (a.k.a. xenharmonic) fretted string instruments. It works similarly to other interactive "scale practice" applications for guitar such as [fretastic](https://fretastic.com/guitar), but for microtonal instruments.

Xen Fret uses data from the [xen wiki](https://en.xen.wiki/w/Main_Page), together with some of my own manually entered data for my own 22-EDO guitar. This application is developed in my free time, and will always be free and open source. If you see anything wrong / missing, or have a feature request, please either [file an issue](https://github.com/Sintrastes/xen-fret/issues/new) or submit a PR! 

Features
--------

  - Manage scales and chords for different instruments. When you first open the app, you'll start by entering the exact configuration of your instrument, even if it's something exotic like a [Kite guitar](https://kiteguitar.com/).
  - Practice scales / chords.
  - Export or print SVG fretboard diagrams.
  - Customize the theme of the application and of the fretboard diagrams.
  - Real-time pitch detection / transcription for microtonal instruments. (WIP)

Gettring started
----------------

Xen Fret is currently hosted [here](https://sintrastes.github.io/demos/xen_fret/) as a static page on github pages. In the future, we may add desktop and mobile versions.

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

The app is served from a subdirectory (`/xen_fret/`), which requires a different base path. Use `Dioxus.gh-pages.toml` for that build:

```
cp Dioxus.gh-pages.toml Dioxus.toml && dx build --release --platform web
```

Nerd Stuff
----------

Xen Fret has been a longstanding project of mine. Originally it was a simple [CGI](https://en.wikipedia.org/wiki/Common_Gateway_Interface) binary that could be served up in Apache and generated static SVG images built in Haskell using [diagrams](https://diagrams.github.io/). Later on I turned it into a simple web app using [reflex](https://reflex-frp.org/). Since then I've re-written the whole thing in Rust with [Dioxus](https://dioxuslabs.com/) and the assistance of agentic AI.

I am a bit sad with the sunsetting of the old Haskell version, but I think ultimately Rust is the right choice for performance ("Rust -> WASM" is much faster than "Haskell -> JS" was in the browser) and ecosystem, allowing us to iterate faster and deliver more features to the Xenharmonic community. 🎸
