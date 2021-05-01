let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {
    config.allowBroken = true;
    overlays = [(import ../../nix/overlay.nix)];
  };
  kv-plugin-test = pkgs.writeShellScriptBin "kv-plugin-test.sh" ''
  #!/usr/bin/env bash
  set -euo pipefail
  cd "$(mktemp -d)"
  echo "Runing tests in $PWD"
  export KV_PLUGIN="${pkgs.haskellPackages.hashicorp-plugin-example-kv-plugin}/bin/kv-plugin"
  echo "-------------------"
  echo "Putting hello=world"
  echo "-------------------"
  "${pkgs.go-plugin-example-grpc}/bin/hashicorp-plugin-example-client" put hello world
  echo "-------------------"
  echo "Getting hello"
  echo "-------------------"
  val=$("${pkgs.go-plugin-example-grpc}/bin/hashicorp-plugin-example-client" get hello)
  if [[ "$val" == "world" ]]; then
    echo "Test Passed!"
  else
    echo "Test Failed! val='$val'"
    exit 1
  fi
  '';
in
pkgs.buildEnv {
  name = "hashicorp-haskell-plugin-kv-plugin-test";
  paths = [ kv-plugin-test ];
}
