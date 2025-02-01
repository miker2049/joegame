{ stdenv, pkgs, fetchFromGitHub, joegamepkgs, ... }:
stdenv.mkDerivation rec {
  pname = "world-zig";
  version = "1.0";

  src = ./.;

  buildInputs = [
    pkgs.zig
    pkgs.zls
    pkgs.raylib
    joegamepkgs.noise
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

  buildPhase = "echo doo doo";

  installPhase = ''
    mkdir -p $out
    echo doo doo > $out/output
  '';
}
