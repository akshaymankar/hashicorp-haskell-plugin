hself: hsuper: {
  data-diverse = hself.callPackage ./haskell/data-diverse.nix {};
  http2-client = hself.callPackage ./haskell/http2-client.nix {};
  http2-client-grpc = hself.callPackage ./haskell/http2-client-grpc.nix {};
  http2-grpc-proto3-wire = hself.callPackage ./haskell/http2-grpc-proto3-wire.nix {};
  mu-avro = hself.callPackage ./haskell/mu-avro.nix {};
  mu-rpc = hself.callPackage ./haskell/mu-rpc.nix {};
  mu-schema = hself.callPackage ./haskell/mu-schema.nix {};
  mu-grpc-server = hself.callPackage ./haskell/mu-grpc-server.nix {};
  mu-grpc-client = hself.callPackage ./haskell/mu-grpc-client.nix {};
  mu-grpc-common = hself.callPackage ./haskell/mu-grpc-common.nix {};
  mu-protobuf = hself.callPackage ./haskell/mu-protobuf.nix {};
  proto3-wire = hself.callPackage ./haskell/proto3-wire.nix {};
  warp-grpc = hself.callPackage ./haskell/warp-grpc.nix {};
  tracing-control = hself.callPackage ./haskell/tracing-control.nix {};
  network = hself.network_3_1_2_7;

  hashicorp-plugin = hself.callPackage ../hashicorp-plugin/default.nix {};
  hashicorp-plugin-example-kv-plugin = hself.callPackage ../examples/kv-plugin/default.nix {};
  hashicorp-plugin-example-bidirectional = hself.callPackage ../examples/bidirectional/default.nix {};
  waypoint-plugin-sdk = hself.callPackage ../waypoint/waypoint-plugin-sdk/default.nix {};
  waypoint-plugin-dummy = hself.callPackage ../waypoint/waypoint-plugin-dummy/default.nix {};
}
