# Xen Fret

<p align="center">
  <a href="https://haskell.org/">
    <img src="https://img.shields.io/badge/Language-Haskell-blue">
  </a>
  <a href="https://github.com/Sintrastes/xen-fret/actions/workflows/build.yml">
    <img src="https://github.com/Sintrastes/xen-fret/actions/workflows/build.yml/badge.svg">
  </a>
</p>

Xen Fret is a simple web app for generating SVG xenharmonic/microtonal scale diagrams for fretted string instruments. Specifically, for generating scale diagrams for equal temperaments. 

Gettring started
----------------

Xen Fret is currently hosted [here](https://sintrastes.github.io/demos/xen_fret/) as a static page on github pages. In the future, we may add desktop and mobile versions.

Building
--------

Xen Fret is built with [reflex-frp](https://reflex-frp.org/) and [nix](https://github.com/NixOS/nix), and can be deployed as a static webpage (with front-end Javascript). To build a static front-end with nix, simply run `nix-build -A ghcjs.frontend`.

Xen fret can also be built as a native executable thanks to [jsaddle](https://github.com/ghcjs/jsaddle). To build Xen Fret this way, simply run:

```
nix-build -A ghc.frontend
```

to build with nix, or 

```
cabal build frontend
```

to build with [cabal](https://www.haskell.org/cabal/).

Example Screenshots
-------------------

The example from above: Major and minor scales for a bass guitar in standard tuning:

<p align="center"><img src="img/hsfret_screenshot.png" width=350></p>

Major and minor scales for a bass in 22 tone equal temperament.

<p align="center"><img src="img/hsfret_screenshot2.png" width=200></p>

Porcupine[7] and Porcupine[8] scales for a bass guitar in 22 tone equal temperament:

<p align="center"><img src="img/hsfret_screenshot3.png" width=200></p>

Mavila[9] scale diagram for a guitar in 16 tone equal temperament:

<p align="center"><img src="img/hsfret_16.png" width=200></p>

Major and minor scales on a mandolin:

<p align="center"><img src="img/hsfret_mandolin_maj_min.png" width=200></p>

Major and minor scales on a guitar:

<p align="center"><img src="img/hsfret_guitar_maj_min.png" width=350></p>
