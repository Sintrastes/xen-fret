{ bootstrap ? import <nixpkgs> {} }:
let
  reflex-platform = bootstrap.fetchFromGitHub {
    owner = "reflex-frp";
    repo  = "reflex-platform";
    rev = "716879f16d53c93766e7ed9af17416fccb2edfe1";
    sha256 = "1mngxa24cfpvxxq4hgh77nw36vny4abl5wi2xmlwpkk25wzm0h0x";
  };
in
  import reflex-platform {}