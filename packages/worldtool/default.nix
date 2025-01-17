{ stdenv, pkgs, joegamepkgs, sbcl, ... }:
let
  worldtool = sbcl.buildASDFSystem rec {
    pname = "worldtool";
    version = "4.0";
    src = ./.;
    asds = [ "worldtool" ];
    lispLibs = with pkgs.sbclPackages; [
      joegamepkgs.world
      alexandria
      cl-liballegro
      cl-liballegro-nuklear
      livesupport
    ];
    nativeLibs = [ pkgs.openssl pkgs.libev ];
  };
  sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit worldtool; });
in sbcl'.pkgs.worldtool
