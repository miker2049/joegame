{ stdenv, pkgs, fetchFromGitHub, joegamepkgs, ... }:
stdenv.mkDerivation rec {
  pname = "world-zig";
  version = "1.0";

  src = ./.;

  buildInputs = [ pkgs.zig pkgs.zls ];

  buildPhase = "echo doo doo";

  installPhase = ''
    mkdir -p $out
    echo doo doo > $out/output
  '';
}
