{ mkDerivation, base, hashicorp-plugin, lib, waypoint-plugin-sdk }:
mkDerivation {
  pname = "waypoint-plugin-dummy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base hashicorp-plugin waypoint-plugin-sdk
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
