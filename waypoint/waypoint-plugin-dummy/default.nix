{ mkDerivation, base, conduit, hashicorp-plugin, lib
, waypoint-plugin-sdk
}:
mkDerivation {
  pname = "waypoint-plugin-dummy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base conduit hashicorp-plugin waypoint-plugin-sdk
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
