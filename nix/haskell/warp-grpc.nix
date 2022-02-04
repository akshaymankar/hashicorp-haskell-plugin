{ mkDerivation, async, base, binary, bytestring, case-insensitive
, http-types, http2, http2-grpc-types, lib, unliftio-core, wai
, warp, warp-tls
}:
mkDerivation {
  pname = "warp-grpc";
  version = "0.4.0.1";
  src = "${(import ../sources.nix).http2-grpc-haskell}/warp-grpc";
  libraryHaskellDepends = [
    async base binary bytestring case-insensitive http-types http2
    http2-grpc-types unliftio-core wai warp warp-tls
  ];
  homepage = "https://github.com/haskell-grpc-native/http2-grpc-haskell#readme";
  description = "A minimal gRPC server on top of Warp";
  license = lib.licenses.bsd3;
}
