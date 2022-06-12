
{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    config.android_sdk.accept_license = true;
  }
}:
with obelisk;
project ./. ({ pkgs, ... }: {

  android.applicationId = "io.github.sintrastes.xenfret";
  android.displayName = "Xen Fret";

  packages = {
    frontend = ./frontend;
    xen-fret = ./xen-fret;
  };

  overrides = self: super: {
    diagrams-lib = pkgs.haskell.lib.dontCheck super.diagrams-lib;
    JuicyPixels = pkgs.haskell.lib.dontCheck super.JuicyPixels;
})
