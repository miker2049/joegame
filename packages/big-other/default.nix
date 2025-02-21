{ stdenv, pkgs, fetchFromGitHub, ... }:
stdenv.mkDerivation rec {
  pname = "big-other";
  version = "1.0";

  src = ./.;

  buildInputs = [
    (pkgs.python3.withPackages (p: [
      p.ebooklib
      p.beautifulsoup4
      p.stanza
      p.transformers
      p.scipy
      p.numpy
    ]))
    pkgs.zulu23
  ];

  buildPhase = ''
    echo doot doot
  '';

  installPhase = ''
    echo doot doot
  '';
}
