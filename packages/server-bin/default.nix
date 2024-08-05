{ stdenv, pkgs, joegamepkgs, fetchFromGitHub, ... }:
let
  server = joegamepkgs.server;
  sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit server; });
in stdenv.mkDerivation rec {
  pname = "joegame-server-bin";
  version = "1.0";

  src = ./.;

  buildInputs = [ sbcl' ];

  buildPhase = ''
    ${sbcl'}/bin/sbcl --script builder.lisp
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./server-bin $out/bin
  '';
}
