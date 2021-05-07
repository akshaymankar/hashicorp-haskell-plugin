project = "waypoint-haskell-plugin-test"

app "test-app" {
  build {
    use "dummy" {}
  }

  # It is invalid without this, even if we don't use it.
  deploy {
    use "docker" {}
  }
}
