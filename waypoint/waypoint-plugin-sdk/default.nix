{ mkDerivation, base, bytestring, conduit, containers, data-has
, generics-sop, hashicorp-plugin, lib, mtl, mu-grpc-client
, mu-grpc-server, mu-optics, mu-protobuf, mu-rpc, mu-schema
, optics-core, stm, text, unordered-containers
}:
mkDerivation {
  pname = "waypoint-plugin-sdk";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit containers data-has generics-sop
    hashicorp-plugin mtl mu-grpc-client mu-grpc-server mu-optics
    mu-protobuf mu-rpc mu-schema optics-core stm text
    unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
