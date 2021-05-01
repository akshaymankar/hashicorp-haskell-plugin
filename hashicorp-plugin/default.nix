{ mkDerivation, async, attoparsec, base, broadcast-chan, bytestring
, conduit, data-has, first-class-families, hashable, http2-client
, http2-client-grpc, http2-grpc-types, mtl, mu-grpc-client
, mu-grpc-server, mu-optics, mu-protobuf, mu-rpc, mu-schema
, network, stdenv, stm, stm-conduit, temporary, text
, unordered-containers, warp, warp-grpc
}:
mkDerivation {
  pname = "hashicorp-plugin";
  version = "0.1.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    async attoparsec base broadcast-chan bytestring conduit data-has
    first-class-families hashable http2-client http2-client-grpc
    http2-grpc-types mtl mu-grpc-client mu-grpc-server mu-optics
    mu-protobuf mu-rpc mu-schema network stm stm-conduit temporary text
    unordered-containers warp warp-grpc
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
