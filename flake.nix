{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.nixpkgs-patched.url = "github:akshaymankar/nixpkgs/ghc9-generic-lens";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, nixpkgs-patched, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {inherit system;};
        pkgs-patched = import nixpkgs-patched {inherit system;};
        ghc9Pkgs = pkgs-patched.haskell.packages.ghc902.override {
          overrides = import ./nix/haskell-overrides.nix;
        };
      in rec {
        packages = rec {
          dev-env =
            pkgs.buildEnv {
              name = "hashicorp-haskell-plugin-dev-env";
              paths = [
                pkgs.pkgconfig
                pkgs.zlib.dev
                pkgs.zlib.out
                pkgs.haskell.compiler.ghc902
                pkgs.haskellPackages.cabal-install
                pkgs.haskell-language-server
                pkgs.haskellPackages.implicit-hie
                pkgs.cabal2nix
                pkgs.waypoint
                pkgs.shellcheck
              ];
            };

          hashicorp-plugin = ghc9Pkgs.hashicorp-plugin;
          hashicorp-plugin-example-kv-plugin = ghc9Pkgs.hashicorp-plugin-example-kv-plugin;
          hashicorp-plugin-example-bidirectional = ghc9Pkgs.hashicorp-plugin-example-bidirectional;
          waypoint-plugin-sdk = ghc9Pkgs.waypoint-plugin-sdk;
          waypoint-plugin-dummy = ghc9Pkgs.waypoint-plugin-dummy;

          go-plugin-example-grpc = pkgs.callPackage ./nix/go-plugin-example-grpc.nix {};

          waypoint-dummy-test-env = import ./waypoint/dummy-test/env.nix {
            inherit pkgs waypoint-plugin-dummy;
            waypoint = pkgs.waypoint;
          };
        };
        defaultPackage = packages.dev-env;
      });
}
