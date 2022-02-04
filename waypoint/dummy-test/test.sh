#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

nix build -o "$SCRIPT_DIR/.test-env" '.#waypoint-dummy-test-env'
"$SCRIPT_DIR/.test-env/bin/waypoint-dummy-plugin-test.sh"

