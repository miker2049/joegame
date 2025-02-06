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
      lparallel
      blackbird
      cl-async
      png
      ningle
      clack
      joegamepkgs.joegame-assets
      nodgui
    ];
    nativeLibs = [
      pkgs.wavefunctioncollapse
      pkgs.sqlite
      pkgs.imagemagick
      pkgs.libffi
      pkgs.libpng
      joegamepkgs.noise
      pkgs.xxHash
      pkgs.tk-9_0

      pkgs.openssl
      pkgs.libev
      (stdenv.mkDerivation {
        name = "sqlite-world";
        version = "4.0";
        src = ./sqlite-world/.;
        buildInputs = [ pkgs.sqlite.dev pkgs.gcc ];

        buildPhase = ''
          gcc -shared -fPIC -Wall -I${pkgs.sqlite.dev}/include/ \
              -o libsqlite-world.so sqlite_world_bulk_insert.c -lsqlite3
        '';

        installPhase = ''
          mkdir -p $out/lib
          cp ./libsqlite-world.so $out/lib/
        '';
      })
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
