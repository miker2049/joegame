{ stdenv, pkgs, fetchFromGitHub, ... }:
stdenv.mkDerivation rec {
  pname = "dwyaaw";
  version = "1.0";
  src = ./.;
  buildInputs = [ pkgs.allegro5 pkgs.sbcl pkgs.libffi ];

  buildPhase = ''
    echo beep
  '';

  installPhase = ''
    mkdir $out
    cat "boop" > $out/boop
  '';
}

# { stdenv, pkgs, joegamepkgs, sbcl, fetchFromGitHub, build-asdf-system, ... }:
# let
#   build-with-compile-into-pwd = args:
#     let
#       build = (build-asdf-system
#         (args // { version = args.version + "-build"; })).overrideAttrs (o: {
#           buildPhase = with builtins; ''
#             mkdir __fasls
#             export ASDF_OUTPUT_TRANSLATIONS="$(pwd):$(pwd)/__fasls:${storeDir}:${storeDir}"
#             export CL_SOURCE_REGISTRY=$CL_SOURCE_REGISTRY:$(pwd)//
#             ${o.pkg}/bin/${o.program} ${
#               toString (o.flags or [ ])
#             } < ${o.buildScript}
#           '';
#           installPhase = ''
#             mkdir -pv $out
#             rm -rf __fasls
#             cp -r * $out
#           '';
#         });
#     in build-asdf-system (args // {
#       # Patches are already applied in `build`
#       patches = [ ];
#       src = build;
#     });
#   cl-liballegro-nuklear' = build-with-compile-into-pwd {
#     pname = "cl-liballegro-nuklear";
#     version = "foo";
#     nativeBuildInputs = [ pkgs.allegro5 ];
#     nativeLibs = [ pkgs.allegro5 ];
#     # lispLibs = super.cl-liballegro-nuklear.lispLibs ++ [ super.cl-liballegro ];
#     patches = [ ];
#   };
#   dark-work-you-are-a-wizard = sbcl.buildASDFSystem rec {
#     pname = "dark-work-you-are-a-wizard";
#     version = "0.0.1";
#     src = ./.;
#     asds = [ "dark-work-you-are-a-wizard.asd" ];
#     lispLibs = with pkgs.sbclPackages; [
#       alexandria
#       cl-liballegro
#       cl-liballegro-nuklear'
#       livesupport
#       deploy
#       joegamepkgs.assets
#     ];
#     nativeLibs = [ pkgs.allegro5 ];
#   };
#   sbcl' = pkgs.sbcl.withOverrides
#     (self: super: { inherit dark-work-you-are-a-wizard; });
# in sbcl'.pkgs.dark-work-you-are-a-wizard
