{ mkDerivation, base, bytestring, cereal, containers, deepseq
, doctest, ghc-prim, hashable, parameterized, primitive, QuickCheck
, safe, lib, tasty, tasty-hunit, tasty-quickcheck, text
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.2.1";
  src = "${(import ../sources.nix).proto3-wire}";
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq ghc-prim hashable
    parameterized primitive QuickCheck safe text transformers
    unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring cereal doctest QuickCheck tasty tasty-hunit
    tasty-quickcheck text transformers vector
  ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = lib.licenses.asl20;
}
