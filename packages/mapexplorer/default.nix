{ stdenv, pkgs, buildNpmPackage, ... }:
buildNpmPackage rec {
  pname = "joegame-mapexplorer";
  version = "1.0";
  src = ./.;
  npmDepsHash = "sha256-O2TNqahU23SpbfZRSABpbLyHSrQiPfDpol8OW3pOWhw=";
}
