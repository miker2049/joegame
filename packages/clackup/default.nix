{ stdenv, pkgs, sbcl, ... }:
stdenv.mkDerivation rec {
  pname = "clackup";
  version = "07/09/24";
  src = ./.;
  buildInputs = [ sbcl ];
  nativeBuildInputs = [ sbcl ];
  installPhase = ''
    mkdir -p $out/bin
    cp $src/clackup $out/bin/clackup
    chmod +x $out/bin/clackup
  '';
}
