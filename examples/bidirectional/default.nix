{ mkDerivation, base, data-has, hashicorp-plugin, mtl
, mu-grpc-client, mu-grpc-server, mu-optics, mu-protobuf, mu-rpc
, mu-schema, stdenv, stm, text, unordered-containers
}:
mkDerivation {
  pname = "hashicorp-plugin-example-bidirectional";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base data-has hashicorp-plugin mtl mu-grpc-client mu-grpc-server
    mu-optics mu-protobuf mu-rpc mu-schema stm text
    unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
