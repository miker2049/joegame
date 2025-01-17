{ stdenv, pkgs, joegamepkgs, sbcl, fetchFromGitHub, ... }:
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
      joegamepkgs.joegame-assets
    ];
    nativeLibs = [
      pkgs.wavefunctioncollapse
      pkgs.sqlite
      pkgs.imagemagick
      pkgs.libffi
      pkgs.libpng
      joegamepkgs.noise
      pkgs.xxHash

      (stdenv.mkDerivation {
        name = "FastNoiseLite";
        version = "v1.1.1";
        src = fetchFromGitHub {
          owner = "Auburn";
          repo = "FastNoiseLite";
          rev = "v1.1.1";
          hash = "sha256-l4FoG2DXHGf8x72NkZi3rA1MLwOG6yTcvropZ0WFuJY=";
        };

        buildPhase = ''
          echo doodoo
        '';

        installPhase = ''
          mkdir -p $out/include
          cp ./C/FastNoiseLite.h $out/include/
        '';
      })
    ];
  };
  sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit world; });
in sbcl'.pkgs.world
