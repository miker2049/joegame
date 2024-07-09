{ stdenv, pkgs, ps, sbcl, ... }:
let
  assets = sbcl.buildASDFSystem rec {
    pname = "assets";
    version = "4.0";
    src = ./.;
    asds = [ "assets" ];
    lispLibs = with pkgs.sbclPackages; [ ];
    nativeLibs = [ ];
  };
  sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit assets; });
in sbcl'.pkgs.assets
