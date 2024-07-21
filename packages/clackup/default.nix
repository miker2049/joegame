{ stdenv, pkgs, roswell, ... }:
stdenv.mkDerivation rec {
  pname = "clackup";
  version = "07/09/24";
  src = ./.;
  buildInputs = [ roswell ];
  nativeBuildInputs = [ roswell ];
  installPhase = ''
    mkdir -p $out/bin
    cp $src/clackup $out/bin/clackup
    chmod +x $out/bin/clackup
  '';
}
