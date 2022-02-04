{ mkDerivation, avro, base, binary, bytestring
, http2-grpc-proto3-wire, http2-grpc-types, mu-avro, mu-protobuf
, mu-rpc, mu-schema, lib
}:
mkDerivation {
  pname = "mu-grpc-common";
  version = "0.4.0.0";
  src = "${(import ../sources.nix).mu-haskell}/grpc/common";
  libraryHaskellDepends = [
    avro base binary bytestring http2-grpc-proto3-wire http2-grpc-types
    mu-avro mu-protobuf mu-rpc mu-schema
  ];
  homepage = "https://higherkindness.io/mu-haskell/";
  description = "gRPC for Mu, common modules for client and server";
  license = lib.licenses.asl20;
}
