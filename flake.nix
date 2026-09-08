{
  description = "Compositor for the COSMIC desktop environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # The revision NixOS 26.11 ships, used only by `devShells.deploy`. The
    # unstable channel above is a moving target, and ld.so rejects a binary
    # that wants a newer glibc symbol version than the device has before it
    # binds anything - so the shell `deploy:nix` packages in is pinned to what
    # the Kora devices actually run.
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
              mesa # For libgbm
              fontconfig
              stdenv.cc.cc.lib
              pixman
              libdisplay-info
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

          # rustc drives the link, but it is the shell's gcc and glibc that
          # decide which symbol versions the compositor ends up asking for, so
          # `deploy:nix` packages against this stdenv rather than the unstable
          # one above.
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

          # What `go-task deploy:nix` builds the RPM in. The library set is the
          # one pkg/nix/dev-package.nix.in names on the device, so what this
          # links against is what autoPatchelfHook resolves there - notably
          # libdisplay-info 0.3, which the container instead links statically to
          # dodge Fedora 43/44 soname skew that nix does not have.
          devShells.deploy = pkgsKora.mkShell {
            name = "cosmic-comp-deploy";

            # Not rust-bin: rust-toolchain.toml pins 1.93 and this flake's
            # rust-overlay input is from 2024-08, which has no manifest that far
            # forward - devShells.default fails on the same line. nixpkgs' own
            # rustc here is 1.97 and a non-rustup cargo ignores the toml, so the
            # pinned channel is a floor this clears rather than a version to match.
            nativeBuildInputs = with pkgsKora; [
              rustc
              cargo
              pkg-config
              cmake
              rustPlatform.bindgenHook # sets LIBCLANG_PATH
              go-task # the inner `task dist:rpm`
              rpm # rpmbuild
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
