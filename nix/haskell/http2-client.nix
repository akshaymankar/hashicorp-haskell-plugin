{ mkDerivation, async, base, bytestring, containers, deepseq, http2
, lib, lifted-async, lifted-base, mtl, network, stm, time, tls
, transformers-base
}:
mkDerivation {
  pname = "http2-client";
  version = "0.10.0.0";
  src = "${(import ../sources.nix).http2-client}";
  libraryHaskellDepends = [
    async base bytestring containers deepseq http2 lifted-async
    lifted-base mtl network stm time tls transformers-base
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/lucasdicioccio/http2-client";
  description = "A native HTTP2 client library";
  license = lib.licenses.bsd3;
}
