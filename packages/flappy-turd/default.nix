{ config, lib, pkgs, ... }:

{
  pname = "flappy-turd";
  version = "1.0";

  src = ./.;

  buildInputs = [ pkgs.godot4 ];

  buildPhase = ''
    mkdir dist
      ${pkgs.godot4}/bin/godot4 --headless --export-release Web dist/flappy-turd
  '';
  installPhase = ''
    mkdir $out
    cp -r dist/* $out/

  '';
}
