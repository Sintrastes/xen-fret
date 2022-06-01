
   
{ reflex-platform ? import ./nix/reflex-platform.nix {}
, withHoogle ? false
, useWarp ? true
}:
reflex-platform.project ({ pkgs, ... }: {

  inherit withHoogle;
  inherit useWarp;

  packages = {
    frontend = ./frontend;
    xen-fret = ./xen-fret;
  };

  shells = {
    ghc = ["frontend"];
    ghcjs = ["frontend"];
  };

  shellToolOverrides = self: super: {
  };

  overrides = self: super: {
      doctest = pkgs.callHackageDirect {
        pkg = "doctest";
        ver = "0.16.3";
        sha256 = "qU50YWyeM1QI3lGQwboJ0iUlC4c4YTOrv3u/aVagRlg=";
      } {};
  };
})