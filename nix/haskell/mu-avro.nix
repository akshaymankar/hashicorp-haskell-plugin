{ mkDerivation, aeson, avro, base, bytestring, containers, deepseq
, language-avro, lib, mu-rpc, mu-schema, sop-core, tagged
, template-haskell, text, time, transformers, unordered-containers
, uuid, vector
}:
mkDerivation {
  pname = "mu-avro";
  version = "0.4.0.4";
  src = "${(import ../sources.nix).mu-haskell}/adapter/avro";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson avro base bytestring containers deepseq language-avro mu-rpc
    mu-schema sop-core tagged template-haskell text time transformers
    unordered-containers uuid vector
  ];
  executableHaskellDepends = [
    avro base bytestring containers mu-schema
  ];
  homepage = "https://higherkindness.io/mu-haskell/";
  description = "Avro serialization support for Mu microservices";
  license = lib.licenses.asl20;
}
