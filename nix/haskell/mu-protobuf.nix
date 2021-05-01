{ mkDerivation, base, bytestring, compendium-client, containers
, http-client, http2-grpc-proto3-wire, language-protobuf, mu-rpc
, mu-schema, proto3-wire, servant-client-core, sop-core, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "mu-protobuf";
  version = "0.4.2.0";
  src = "${(import ../sources.nix).mu-haskell}/adapter/protobuf";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring compendium-client containers http-client
    http2-grpc-proto3-wire language-protobuf mu-rpc mu-schema
    proto3-wire servant-client-core sop-core template-haskell text
  ];
  executableHaskellDepends = [
    base bytestring containers mu-schema proto3-wire text
  ];
  homepage = "https://higherkindness.io/mu-haskell/";
  description = "Protocol Buffers serialization and gRPC schema import for Mu microservices";
  license = stdenv.lib.licenses.asl20;
}
