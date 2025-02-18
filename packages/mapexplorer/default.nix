{ stdenv, pkgs, joegamepkgs, buildNpmPackage, ... }:
buildNpmPackage rec {
  pname = "joegame-mapexplorer";
  version = "1.0";
  src = ./.;
  npmDepsHash = "sha256-f5waBPJe7QRzqYKux1YfhduH5H87SaLTauWoEJWL3Ug=";
  nativeBuildInputs = [ joegamepkgs.world ];
}
