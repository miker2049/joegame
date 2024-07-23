{ stdenv, pkgs, fetchFromGitHub, ... }:
stdenv.mkDerivation rec {
  pname = "joegame-noise-libs";
  version = "1.0";

  src = ./.;

  buildInputs = [
    pkgs.emscripten
    pkgs.nodePackages.pnpm
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

  buildPhase = ''
    make libs
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp ./libspooky.so $out/lib/libspooky.so
    cp ./libsimplex.so $out/lib/libsimplex.so
    cp ./libfastnoiselite.so $out/lib/libfastnoiselite.so
  '';
}
