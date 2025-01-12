{ stdenv, pkgs, joegamepkgs, ... }:
stdenv.mkDerivation rec {
  pname = "flappy-turd";
  version = "1.0";

  src = ./.;

  buildInputs = [ pkgs.godot_4 ];

  buildPhase = ''
    mkdir dist
    ${pkgs.godot_4}/bin/godot4 --headless --editor --quit
    ${pkgs.godot_4}/bin/godot4 --headless --export-release Web dist/flappy-turd
  '';
  installPhase = ''
    mkdir $out
    cp -r dist/* $out/

  '';
}
