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

Contributing
------------

Default tuning for an EDO doesn't make sense? Default set of scales looking a bit sparse for your favorite temperament? Find an error in the data set of scales/tunings/temperaments? Either [file an issue](https://github.com/Sintrastes/xen-fret/issues/new) to let me know, or (if you have some techincal knowledge) contribute a fix yourself. The default dataset bundled with the app can be found [here](https://github.com/Sintrastes/xen-fret/blob/master/xen-fret/src/XenFret/AppData.hs#L30).

Interested in contributing to some more technical issues but don't know where to start? Feel free to [reach out](mailto:nbedell@tulane.edu).

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

Like this project?
------------------

If you use Xen Fret, or find it useful, consider supporting it's development efforts by buying me a coffee ☕, or a beer 🍺!

[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/donate?business=45F7QR92B4XUY&no_recurring=0&currency_code=USD)
[![Donate with Ethereum](https://en.cryptobadges.io/badge/micro/0x61531fCA114507138ebefc74Db5C152845b77Cad)](https://en.cryptobadges.io/donate/0x61531fCA114507138ebefc74Db5C152845b77Cad)
