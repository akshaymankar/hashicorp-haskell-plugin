{buildGoModule, fetchFromGitHub, stdenv}:
let sources = import ./sources.nix;
    # This hack is required because buildGoModule blindly ignores anything which
    # has 'examples' in its path.
    modifiedSource = stdenv.mkDerivation {
      name = "hashicorp-go-plugin-source-modified";
      src = sources.hashicorp-go-plugin;
      installPhase = ''
        mkdir -p $out
        cp -r $src/. $out/.
        cp -r $out/examples $out/foo
      '';
    };
in buildGoModule rec {
  pname = "go-plugin-example-grpc";
  version = "1.4.1";
  src = modifiedSource;
  vendorSha256 = "sha256:1q54rvvphin5rnpb1n75baby5rqkakp9hsyzv3bf9p6yy9p5f17q";
  subPackages = [ "foo/grpc" "foo/bidirectional" ];
  postInstall = ''
    mv $out/bin/grpc $out/bin/hashicorp-plugin-example-client
    mv $out/bin/bidirectional $out/bin/hashicorp-plugin-example-bidirectional-client
  '';
}
