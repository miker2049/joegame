{ stdenv, pkgs, ... }:
stdenv.mkDerivation rec {
  pname = "joegame-noise-libs";
  version = "1.0";

  src = ./.;

  buildInputs = [ pkgs.bun ];

  buildPhase = ''
    make libsimplex.so
    make libspooky.so
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp ./libspooky.so $out/lib/libspooky.so
    cp ./libsimplex.so $out/lib/libsimplex.so
  '';
}
