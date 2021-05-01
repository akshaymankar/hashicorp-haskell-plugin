# Bidirectional Plugin

## How to test

1. Get the `go-plugin` repo
   ```bash
   git clone https://github.com/hashicorp/go-plugin
   ```
1. Build this plugin
   ```bash
   cabal install hashicorp-plugin-example-bidirectional
   ```
1. Persist something. Run this from `go-plugin/examples/bidirectional/`
   ```bash
   COUNTER_PLUGIN=~/.cabal/bin/hashicorp-plugin-example-bidirectional go run . put hello 2
   ```
1. Persist something again. Run this from `go-plugin/examples/bidirectional/`
   ```bash
   COUNTER_PLUGIN=~/.cabal/bin/hashicorp-plugin-example-bidirectional go run . put hello 3
   ```
1. Retrieve the thing. Run this from `go-plugin/examples/grpc/`
   ```bash
   COUNTER_PLUGIN=~/.cabal/bin/hashicorp-plugin-example-bidirectional go run . get hello
   ```
   This should return `5`
