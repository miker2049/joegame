{ stdenv, pkgs, fetchFromGitHub, ... }:
stdenv.mkDerivation rec {
  pname = "joegame-mapexplorer";
  version = "1.0";

  src = ./.;

  buildInputs = [ pkgs.nodePackages.prettier pkgs.nodePackages.pnpm ];

  buildPhase = ''
    pnpm install
  '';

  installPhase = ''
    mkdir -p $out
    touch $out/dummy
  '';
}
