{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {inherit system;};
      in rec {
        packages.dev-env = pkgs.buildEnv {
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
        defaultPackage = packages.dev-env;
    });
}
