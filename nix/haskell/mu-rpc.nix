{ mkDerivation, aeson, base, conduit, http-types, mtl, mu-schema
, sop-core, lib, template-haskell, text, wai
}:
mkDerivation {
  pname = "mu-rpc";
  version = "0.5.0.1";
  src = "${(import ../sources.nix).mu-haskell}/core/rpc";
  libraryHaskellDepends = [
    aeson base conduit http-types mtl mu-schema sop-core
    template-haskell text wai
  ];
  homepage = "https://higherkindness.io/mu-haskell/";
  description = "Protocol-independent declaration of services and servers";
  license = lib.licenses.asl20;
}
