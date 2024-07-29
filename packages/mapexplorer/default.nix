{ stdenv, pkgs, buildNpmPackage, ... }:
buildNpmPackage rec {
  pname = "joegame-mapexplorer";
  version = "1.0";
  src = ./.;
  npmDepsHash = "sha256-F0DHz2OZD9j50xWpSvG6rN0MCd0sQ1R88Rf4wQ0zuvc=";
}
