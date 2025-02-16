{ stdenv, pkgs, fetchFromGitHub, joegamepkgs, ... }:
stdenv.mkDerivation rec {
  pname = "world-zig";
  version = "1.0";

  src = ./.;

  buildInputs = with pkgs; [ zig zls ];

  buildPhase = "echo doo doo";

  installPhase = ''
    mkdir -p $out
    echo doo doo > $out/output
  '';
}
