{ mkDerivation, aeson, base, bytestring, containers
, first-class-families, sop-core, stdenv, template-haskell, text
, th-abstraction, unordered-containers, uuid, vector
}:
mkDerivation {
  pname = "mu-schema";
  version = "0.3.1.2";
  src = "${(import ../sources.nix).mu-haskell}/core/schema";
  libraryHaskellDepends = [
    aeson base bytestring containers first-class-families sop-core
    template-haskell text th-abstraction unordered-containers uuid
    vector
  ];
  homepage = "https://higherkindness.io/mu-haskell/";
  description = "Format-independent schemas for serialization";
  license = stdenv.lib.licenses.asl20;
}
