
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
    zlib = pkgs.haskell.lib.dontCheck super.zlib;
  };
})