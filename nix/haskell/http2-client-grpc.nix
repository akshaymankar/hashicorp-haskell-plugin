{ mkDerivation, async, base, binary, bytestring, case-insensitive
, data-default-class, http2, http2-client, http2-grpc-types
, lifted-async, lifted-base, network, lib, text, tls, either
}:
mkDerivation {
  pname = "http2-client-grpc";
  version = "0.8.0.0";
  src = "${(import ../sources.nix).http2-grpc-haskell}/http2-client-grpc";
  libraryHaskellDepends = [
    async base binary bytestring case-insensitive data-default-class
    http2 http2-client http2-grpc-types lifted-async lifted-base
    network text tls either
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/haskell-grpc-native/http2-grpc-haskell/blob/master/http2-client-grpc/README.md";
  description = "Implement gRPC-over-HTTP2 clients";
  license = lib.licenses.bsd3;
}
