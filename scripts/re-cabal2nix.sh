#!/usr/bin/env bash

set -euo pipefail
set -x

ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." &> /dev/null && pwd )"

declare -a projects

projects=(hashicorp-plugin examples/bidirectional examples/kv-plugin waypoint/waypoint-plugin-sdk waypoint/waypoint-plugin-dummy)

for p in "${projects[@]}"; do
    pushd "$ROOT_DIR/$p"
    cabal2nix . > default.nix
    popd
done
