{ stdenv, pkgs, ps, sbcl, ... }:
let
  world = sbcl.buildASDFSystem rec {
    pname = "world";
    version = "4.0";
    src = ./.;
    asds = [ "world" ];
    lispLibs = with pkgs.sbclPackages; [
      deploy
      alexandria
      sqlite
      cl-tiled
      colored
      cffi-libffi
      envy
      clingon
      ironclad
      jonathan
      bordeaux-threads
      blackbird
      cl-async
      png
    ];
    nativeLibs = [
      pkgs.sqlite
      pkgs.imagemagick
      pkgs.libffi
      pkgs.libpng
      ps.noise
      pkgs.xxHash
    ];
  };
  sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit world; });
in sbcl'.pkgs.world
