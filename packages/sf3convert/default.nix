{ stdenv, pkgs, ... }:
stdenv.mkDerivation rec {
  pname = "sf3convert";
  version = "10/11/20";
  src = pkgs.fetchFromGitHub {
    owner = "musescore";
    repo = "sftools";
    rev = "3bcf19183102e9d30c3d84ff1ecb608d31d1369c";
    sha256 = "sha256-KJKecx8XWseTrXI8PT8uBJtcWTivFO14RhOxFzkdE1o=";
  };
  buildInputs = with pkgs; [ libsndfile libogg libvorbis qt5.qtbase ];
  nativeBuildInputs =
    [ pkgs.gnumake pkgs.cmake pkgs.pkg-config pkgs.qt5.wrapQtAppsHook ];
  cmakeFlags = [ "-DCMAKE_BUILD_TYPE=RELEASE" ];
}
