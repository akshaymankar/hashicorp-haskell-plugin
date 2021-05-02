{ mkDerivation, base, bytestring, hashicorp-plugin, lib, mtl
, mu-grpc-server, mu-optics, mu-protobuf, mu-rpc, mu-schema, text
, unordered-containers
}:
mkDerivation {
  pname = "kv-plugin";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base bytestring hashicorp-plugin mtl mu-grpc-server mu-optics
    mu-protobuf mu-rpc mu-schema text unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
