let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.buildEnv {
  name = "hashicorp-haskell-plugin";
  paths = [
    pkgs.pkgconfig
    pkgs.zlib
    pkgs.haskell.compiler.ghc8104
    pkgs.haskellPackages.cabal-install
    pkgs.haskell-language-server
    pkgs.haskellPackages.implicit-hie
  ];
}
