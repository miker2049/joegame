{ stdenv, pkgs, joegamepkgs, sbcl, fetchFromGitHub, ... }:
let
  golf-2d = sbcl.buildASDFSystem rec {
    pname = "golf-2d";
    version = "4.0";
    src = ./.;
    asds = [ "golf-2d" ];
    lispLibs = with pkgs.sbclPackages; [
      cl-liballegro
      cl-liballegro-nuklear
      joegamepkgs.assets
    ];
    nativeLibs = [ pkgs.libffi joegamepkgs.noise pkgs.allegro5 ];
  };
  sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit golf-2d; });
in sbcl'.pkgs.golf-2d
