{ mkDerivation, async, avro, base, bytestring, conduit, http2
, http2-client, http2-client-grpc, http2-grpc-types, mu-grpc-common
, mu-optics, mu-protobuf, mu-rpc, mu-schema, optics-core, sop-core
, lib, stm, stm-chans, stm-conduit, template-haskell, text
, th-abstraction, tracing, tracing-control
}:
mkDerivation {
  pname = "mu-grpc-client";
  version = "0.4.0.2";
  src = "${(import ../sources.nix).mu-haskell}/grpc/client";
  libraryHaskellDepends = [
    async avro base bytestring conduit http2 http2-client
    http2-client-grpc http2-grpc-types mu-grpc-common mu-optics
    mu-protobuf mu-rpc mu-schema optics-core sop-core stm stm-chans
    stm-conduit template-haskell text th-abstraction tracing tracing-control
  ];
  homepage = "https://higherkindness.io/mu-haskell/";
  description = "gRPC clients from Mu definitions";
  license = lib.licenses.asl20;
}
