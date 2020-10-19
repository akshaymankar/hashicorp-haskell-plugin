let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
with pkgs; mkShell {
  name = "wire-cli";
  buildInputs = [
    pkgconfig
    zlib
    haskell.compiler.ghc884
    haskellPackages.cabal-install
    ncurses
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${ncurses}/lib:${zlib}/lib
    '';
}
