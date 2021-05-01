{ mkDerivation, base, binary, bytestring, case-insensitive, hpack
, http2-grpc-types, proto3-wire, stdenv, zlib
}:
mkDerivation {
  pname = "http2-grpc-proto3-wire";
  version = "0.1.0.1";
  src = "${(import ../sources.nix).http2-grpc-haskell}/http2-grpc-proto3-wire";
  libraryHaskellDepends = [
    base binary bytestring case-insensitive http2-grpc-types
    proto3-wire zlib
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/haskell-grpc-native/http2-grpc-haskell#readme";
  description = "Encoders based on `proto3-wire` for gRPC over HTTP2";
  license = stdenv.lib.licenses.bsd3;
}
