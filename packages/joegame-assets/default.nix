{ stdenv, pkgs, ps, sbcl, ... }:
let
  joegame-assets = sbcl.buildASDFSystem rec {
    pname = "joegame-assets";
    version = "4.0";
    src = ./.;
    asds = [ "joegame-assets" ];
    lispLibs = with pkgs.sbclPackages; [ ];
    nativeLibs = [ ];
  };
  sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit joegame-assets; });
in sbcl'.pkgs.joegame-assets
