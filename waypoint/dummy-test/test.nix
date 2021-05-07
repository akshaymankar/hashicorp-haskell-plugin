let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {
    config.allowBroken = true;
    overlays = [(import ../../nix/overlay.nix)];
  };
  waypoint-dummy-plugin-test-app = pkgs.srcOnly {
    name = "waypoint-dummy-plugin-test-app";
    src = ./app;
  };
  waypoint-dummy-plugin-test = pkgs.writeShellScriptBin "waypoint-dummy-plugin-test.sh" ''
  #!/usr/bin/env bash
  set -euo pipefail

  export PATH=${pkgs.waypoint}/bin:${pkgs.haskellPackages.waypoint-plugin-dummy}/bin:$PATH
  ${waypoint-dummy-plugin-test-app}/run.sh
  '';
in
pkgs.buildEnv {
  name = "hashicorp-haskell-plugin-kv-plugin-test";
  paths = [ waypoint-dummy-plugin-test ];
}
