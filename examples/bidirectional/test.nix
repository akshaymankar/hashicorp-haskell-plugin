let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {
    config.allowBroken = true;
    overlays = [(import ../../nix/overlay.nix)];
  };
  bidirectional-plugin-test = pkgs.writeShellScriptBin "bidirectional-plugin-test.sh" ''
  #!/usr/bin/env bash
  set -euo pipefail
  cd "$(mktemp -d)"
  echo "Runing tests in $PWD"
  export COUNTER_PLUGIN="${pkgs.haskellPackages.hashicorp-plugin-example-bidirectional}/bin/hashicorp-plugin-example-bidirectional"
  echo "-------------------"
  echo "Putting hello=2"
  echo "-------------------"
  "${pkgs.go-plugin-example-grpc}/bin/hashicorp-plugin-example-bidirectional-client" put hello 2
  echo "-------------------"
  echo "Putting hello=3"
  echo "-------------------"
  "${pkgs.go-plugin-example-grpc}/bin/hashicorp-plugin-example-bidirectional-client" put hello 3
  echo "-------------------"
  echo "Getting hello"
  echo "-------------------"
  # val=$("${pkgs.go-plugin-example-grpc}/bin/hashicorp-plugin-example-client" get hello)
  val=$("${pkgs.go-plugin-example-grpc}/bin/hashicorp-plugin-example-bidirectional-client" get hello)
  if [[ "$val" == "5" ]]; then
    echo "Test Passed!"
  else
    echo "Test Failed! val='$val'"
    exit 1
  fi
  '';
in
pkgs.buildEnv {
  name = "hashicorp-haskell-plugin-kv-plugin-test";
  paths = [ bidirectional-plugin-test ];
}
