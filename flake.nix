{
  description = "xen-fret development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
          config.android_sdk.accept_license = true;
          config.allowUnfree = true;
        };
        pkgs-unstable = import nixpkgs-unstable { inherit system; };

        rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

        # Android SDK — AGP 8.8.2, compileSdk 35, NDK r27 for Rust/UniFFI cross-compilation
        androidCompose = pkgs.androidenv.composeAndroidPackages {
          buildToolsVersions   = [ "35.0.0" ];
          platformVersions     = [ "35" ];
          includeNDK           = true;
          ndkVersions          = [ "27.0.12077973" ];
          cmdLineToolsVersion  = "13.0";
        };

        androidSdkPath = "${androidCompose.androidsdk}/libexec/android-sdk";
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            pkg-config
            rustToolchain
            pkgs-unstable.wasm-bindgen-cli
          ];

          buildInputs = with pkgs; [
            # GTK4 / libadwaita (xen-fret-gtk)
            gtk4
            libadwaita
            glib
            cairo
            pango
            gdk-pixbuf
            graphene

            openssl

            # cpal / rodio (native audio + mic capture on Linux)
            alsa-lib
          ];

          packages = with pkgs; [
            pkgs-unstable.dioxus-cli

            # Android / Gradle
            jdk17
            gradle
            kotlin
            androidCompose.androidsdk
            cargo-ndk

            cargo-watch
            bacon
          ];

          env = {
            PKG_CONFIG_PATH  = "${pkgs.openssl.dev}/lib/pkgconfig:${pkgs.alsa-lib.dev}/lib/pkgconfig";
            XDG_DATA_DIRS    = "${pkgs.gtk4}/share:${pkgs.libadwaita}/share:$XDG_DATA_DIRS";
            ANDROID_HOME     = androidSdkPath;
            ANDROID_SDK_ROOT = androidSdkPath;
            ANDROID_NDK_ROOT = "${androidSdkPath}/ndk/27.0.12077973";
            JAVA_HOME        = "${pkgs.jdk17}";
          };

          shellHook = ''
            echo "xen-fret dev shell (nightly Rust + GTK4 + Dioxus + Android)"
            echo "  dx serve --package xen-fret                      — web dev server"
            echo "  cargo run -p xen-fret-gtk                        — GTK frontend"
            echo "  cd app/android && ./gradlew assembleDebug        — Android APK"
          '';
        };
      }
    );
}
