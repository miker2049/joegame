{ stdenv, pkgs, joegamepkgs, sbcl, ... }:
let
  spinneret' = sbcl.pkgs.spinneret.overrideLispAttrs (oldAttrs: {
    systems = oldAttrs.systems ++ [ "spinneret/cl-markdown" ];
    lispLibs = oldAttrs.lispLibs ++ [ sbcl.pkgs.cl-markdown ];
  });
  server = sbcl.buildASDFSystem rec {
    pname = "server";
    version = "4.0";
    src = ./.;
    asds = [ "server" ];
    lispLibs = with pkgs.sbclPackages; [
      clack
      lack
      lack-middleware-accesslog
      lack-component
      envy
      cl-ppcre
      caveman2
      png
      prove
      prove-asdf
      cl-who
      cl-syntax-annot
      parenscript
      djula
      spinneret'
      datafly
      sxql
      dbd-sqlite3
      zip
      joegamepkgs.world
      cl-async
    ];
    nativeLibs = [ pkgs.openssl pkgs.libev ];
  };
  sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit server; });
in sbcl'.pkgs.server
