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
      cl-opengl
      glkit
      glkit-examples
      livesupport
      dissect
    ];
    nativeLibs = [ pkgs.openssl pkgs.libev pkgs.mesa ] ++ (with pkgs; [

      pkg-config
      libGL
      wayland
      libxkbcommon
      xorg.libXrandr
      xorg.libXinerama
      xorg.libXcursor
      xorg.libXi
      zig
      zls
      raylib
      raygui
    ]);
  };
  sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit worldtool; });
in sbcl'.pkgs.worldtool
