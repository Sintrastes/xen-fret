{
  description = "xen-fret development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
          config.android_sdk.accept_license = true;
          config.allowUnfree = true;
        };

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
            wasm-bindgen-cli
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
          ];

          packages = with pkgs; [
            # Dioxus CLI — must be 0.7.x; if nixpkgs lags: cargo install dioxus-cli@0.7.3
            dioxus-cli

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
            PKG_CONFIG_PATH  = "${pkgs.openssl.dev}/lib/pkgconfig";
            XDG_DATA_DIRS    = "${pkgs.gtk4}/share:${pkgs.libadwaita}/share:$XDG_DATA_DIRS";
            ANDROID_HOME     = androidSdkPath;
            ANDROID_SDK_ROOT = androidSdkPath;
            ANDROID_NDK_ROOT = "${androidSdkPath}/ndk/27.0.12077973";
            JAVA_HOME        = "${pkgs.jdk17}";
          };

          shellHook = ''
            echo "xen-fret dev shell (nightly Rust + GTK4 + Dioxus + Android)"
            echo "  dx serve                                         — web dev server"
            echo "  cargo run -p xen-fret-gtk                        — GTK frontend"
            echo "  cd app/android && ./gradlew assembleDebug        — Android APK"
            echo "  cargo ndk -t arm64-v8a build -p uniffi_frontend_lib — Rust Android lib"
            echo "  cd app/android && ./gradlew generateUniFFIBindings  — generate Kotlin bindings"
          '';
        };
      }
    );
}
