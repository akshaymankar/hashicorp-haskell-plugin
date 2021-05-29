#!/usr/bin/env bash
set -euo pipefail

source_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

server_dir=$(mktemp -d)
pushd "$server_dir" >/dev/null
  echo "Starting waypoint server in $PWD"

  waypoint server run -accept-tos >"$server_dir/log" &
  # shellcheck disable=SC2064
  trap "kill $!" EXIT

  token=$(waypoint server bootstrap -server-tls-skip-verify)
  export WAYPOINT_TOKEN="$token"
popd >/dev/null

test_dir=$(mktemp -d)
cp "$source_dir/waypoint.hcl" "$test_dir/waypoint.hcl"
dummy_bin=$(which waypoint-plugin-dummy)
cat <<EOF >"$test_dir/waypoint-plugin-dummy"
#!/usr/bin/env bash
"$dummy_bin" 2>$test_dir/build-log.stderr | tee "$test_dir/build-log.stdout"
EOF
chmod +x "$test_dir/waypoint-plugin-dummy"

echo "Running tests in $test_dir"

pushd "$test_dir" >/dev/null
  echo "Initializing"
  waypoint init
  echo "Building"
  waypoint build
  echo "Build successful"
popd >/dev/null
echo "Build stdout:"
cat "$test_dir/build-log.stdout"
echo "Build stderr:"
cat "$test_dir/build-log.stderr"
