
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
      doctest = self.callHackageDirect {
        pkg = "doctest";
        ver = "0.16.3";
        sha256 = "qU50YWyeM1QI3lGQwboJ0iUlC4c4YTOrv3u/aVagRlg=";
      } {};
    };
})