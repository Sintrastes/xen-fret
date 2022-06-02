
{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
  }
}:
with obelisk;
project ./. ({ pkgs, ... }: {

  packages = {
    frontend = ./frontend;
    xen-fret = ./xen-fret;
  };

  overrides = self: super: {
    diagrams-lib = pkgs.haskell.lib.dontCheck super.diagrams-lib;
    JuicyPixels = pkgs.haskell.lib.dontCheck super.JuicyPixels;
    zlib = self.callHackageDirect {
        pkg = "zlib";
        ver = "0.6.3.0";
        sha256 = "qU50YWyeM1QI3lGQwboJ0iUlC4c4YTOrv3u/aVagRlg=";
      } {};
  };
})