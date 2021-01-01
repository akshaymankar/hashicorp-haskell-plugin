let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  hls = pkgs.haskell-language-server.override { supportedGhcVersions = ["8103"]; };
in
with pkgs; mkShell {
  name = "wire-cli";
  buildInputs = [
    pkgconfig
    zlib
    haskell.compiler.ghc8103
    haskellPackages.cabal-install
    ncurses
    hls
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${ncurses}/lib:${zlib}/lib
    '';
}
