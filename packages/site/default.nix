{ stdenv, pkgs, fetchFromGitHub, joegamepkgs, ... }:
stdenv.mkDerivation rec {
  pname = "site";
  version = "1.0";

  src = ./.;

  buildInputs = [ pkgs.emacs ];

  buildPhase = "make";

  installPhase = ''
    mkdir -p $out
    cp -r dist/* $out/
  '';
}
