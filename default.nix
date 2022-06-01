
   
{ reflex-platform ? import ./nix/reflex-platform.nix {}
, withHoogle ? false
, useWarp ? true
}:
reflex-platform.project ({ pkgs, ... }: {

  inherit withHoogle;
  inherit useWarp;

  packages = {
    frontend = ./frontend;
    common = ./common;
  };

  shells = {
    ghc = ["frontend"];
    ghcjs = ["frontend"];
  };

  shellToolOverrides = self: super: {
  };

  overrides = self: super: {
  };
})