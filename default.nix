
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
    };
})