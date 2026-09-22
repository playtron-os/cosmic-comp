{
  description = "Compositor for the COSMIC desktop environment";

  inputs = {
    # Pinned to the running system's nixpkgs: a GL app must use the same mesa as
    # /run/opengl-driver, which is impure system state outside the flake closure.
    nixpkgs.url = "github:NixOS/nixpkgs/34ab99075ac4f7e40cf037eef32cb1c360bb85e9";

    # For `devShells.deploy` only: the revision the devices run, so a build
    # links against their glibc rather than whatever unstable moved to.
    nixpkgs-kora.url = "github:NixOS/nixpkgs/b7c2ada94fe99c15b0dbcf4d11fd7850b957a436";

    parts.url = "github:hercules-ci/flake-parts";
    parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    crane.url = "github:ipetkov/crane";

    rust.url = "github:oxalica/rust-overlay";
    rust.inputs.nixpkgs.follows = "nixpkgs";

    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      parts,
      crane,
      rust,
      nix-filter,
      ...
    }:
    parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-linux"
        "x86_64-linux"
      ];

      perSystem =
        {
          self',
          lib,
          system,
          ...
        }:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend rust.overlays.default;
          rust-toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
          craneLib = (crane.mkLib pkgs).overrideToolchain rust-toolchain;
          craneArgs = {
            pname = "cosmic-comp";
            version = self.rev or "dirty";

            src = nix-filter.lib.filter {
              root = ./.;
              include = [
                ./src
                ./i18n.toml
                ./Cargo.toml
                ./Cargo.lock
                ./resources
                ./cosmic-comp-config
              ];
            };

            nativeBuildInputs = with pkgs; [
              pkg-config
              autoPatchelfHook
              cmake
            ];

            buildInputs = with pkgs; [
              wayland
              systemd # For libudev
              seatd # For libseat
              libxkbcommon
              libinput
              libgbm
              fontconfig
              stdenv.cc.cc.lib
              pixman
              libdisplay-info_0_3 # the -sys crate requires < 0.4
            ];

            runtimeDependencies = with pkgs; [
              libglvnd # For libEGL
              wayland # winit->wayland-sys wants to dlopen libwayland-egl.so
              # for running in X11
              xorg.libX11
              xorg.libXcursor
              xorg.libxcb
              xorg.libXi
              libxkbcommon
              # for vulkan backend
              vulkan-loader
            ];
          };

          cargoArtifacts = craneLib.buildDepsOnly craneArgs;
          cosmic-comp = craneLib.buildPackage (craneArgs // { inherit cargoArtifacts; });

          # The shell's gcc and glibc decide which symbol versions the binary
          # asks for, so `deploy:nix` packages against this stdenv.
          pkgsKora = import inputs.nixpkgs-kora { inherit system; };
        in
        {
          apps.cosmic-comp = {
            type = "app";
            program = lib.getExe self'.packages.default;
          };

          checks.cosmic-comp = cosmic-comp;
          packages.default = cosmic-comp;

          devShells.default = craneLib.devShell {
            LD_LIBRARY_PATH = lib.makeLibraryPath (
              __concatMap (d: d.runtimeDependencies) (__attrValues self'.checks)
            );

            # include build inputs
            inputsFrom = [ cosmic-comp ];
          };

          # What `deploy:nix` builds the RPM in. The library set matches what the
          # device resolves, notably libdisplay-info 0.3.
          devShells.deploy = pkgsKora.mkShell {
            name = "cosmic-comp-deploy";

            # Not rust-bin: the rust-overlay pin has no manifest for 1.93. A
            # non-rustup cargo ignores rust-toolchain.toml, and 1.97 clears it.
            nativeBuildInputs = with pkgsKora; [
              rustc
              cargo
              pkg-config
              cmake
              rustPlatform.bindgenHook # sets LIBCLANG_PATH
              go-task # the inner `task dist:rpm`
              rpm # rpmbuild
              patchelf # remove dev-shell RPATHs from the packaged binary
            ];

            buildInputs = with pkgsKora; [
              libdisplay-info_0_3
              libgbm
              libinput
              pixman
              seatd
              systemd # libudev
              libxkbcommon
              wayland
              fontconfig
            ];

            # dlopen'd, so they are on no link line.
            LD_LIBRARY_PATH = lib.makeLibraryPath (
              with pkgsKora;
              [
                libglvnd
                vulkan-loader
                wayland
                libxkbcommon
              ]
            );
          };
        };
    };
}
