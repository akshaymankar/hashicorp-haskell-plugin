#!/usr/bin/env bash

set -euo pipefail
set -x

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

declare -a projects

projects=(hashicorp-plugin examples/bidirectional examples/kv-plugin waypoint-plugin-sdk)

for p in ${projects[@]}; do
    pushd $p
    cabal2nix . > default.nix
    popd
done
