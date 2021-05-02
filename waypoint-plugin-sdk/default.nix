{ mkDerivation, base, bytestring, containers, hashicorp-plugin, lib
, mu-grpc-server, mu-optics, mu-protobuf, mu-rpc, mu-schema, text
, unordered-containers
}:
mkDerivation {
  pname = "waypoint-plugin-sdk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers hashicorp-plugin mu-grpc-server
    mu-optics mu-protobuf mu-rpc mu-schema text unordered-containers
  ];
  executableHaskellDepends = [ base hashicorp-plugin ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
