{ mkDerivation, async, avro, base, binary, bytestring, conduit
, http2-grpc-types, mtl, mu-grpc-common, mu-protobuf, mu-rpc
, mu-schema, sop-core, stdenv, stm, stm-conduit, wai, warp
, warp-grpc, warp-tls
}:
mkDerivation {
  pname = "mu-grpc-server";
  version = "0.5.0.0";
  src = "${(import ../sources.nix).mu-haskell}/grpc/server";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async avro base binary bytestring conduit http2-grpc-types mtl
    mu-grpc-common mu-protobuf mu-rpc mu-schema sop-core stm
    stm-conduit wai warp warp-grpc warp-tls
  ];
  executableHaskellDepends = [
    async avro base binary bytestring conduit http2-grpc-types mtl
    mu-grpc-common mu-protobuf mu-rpc mu-schema sop-core stm
    stm-conduit wai warp warp-grpc warp-tls
  ];
  homepage = "https://higherkindness.io/mu-haskell/";
  description = "gRPC servers for Mu definitions";
  license = stdenv.lib.licenses.asl20;
}
