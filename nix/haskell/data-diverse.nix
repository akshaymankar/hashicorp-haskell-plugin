{ mkDerivation, base, containers, criterion, deepseq, ghc-prim
, hspec, lib, tagged, hashable
}:
mkDerivation {
  pname = "data-diverse";
  version = "4.7.0.0";
  src = "${(import ../sources.nix).data-diverse}";
  libraryHaskellDepends = [
    base containers deepseq ghc-prim tagged hashable
  ];
  testHaskellDepends = [ base hspec tagged ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/louispan/data-diverse#readme";
  description = "Extensible records and polymorphic variants";
  license = lib.licenses.bsd3;
}
