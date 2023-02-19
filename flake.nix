{
  description = "my project description";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        emacs = ((pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages
          (epkgs: [ epkgs.f epkgs.web-server epkgs.emacsql epkgs.htmlize ]));
        emacss = pkgs.writeShellScriptBin "emacss" "exec ${emacs}/bin/emacs $@";
        sf3convert = pkgs.stdenv.mkDerivation {
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
        };
      in {
        packages.emacss = emacss;
        packages.make = pkgs.gnumake;
        packages.sf3convert = sf3convert;
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs-18_x
            lilypond
            sf3convert
            nodePackages.pnpm
            nodePackages.prettier
            nodePackages.typescript-language-server
            nodePackages.typescript
            deno
            python3
            sqlite
            sbcl
            SDL2
            libffi
            SDL2_image
            SDL2_ttf
            libGL
            emscripten
            gnumake
            coreutils
            bash
            clang
            emacss
            tiled
            # node canvas
            cairo
            pango
            libjpeg
            giflib
            netsurf.libsvgtiny
            libuuid
            imagemagick
            rclone
            gnumake
          ];
          shellHook = with pkgs; ''
            echo "Welcome, mike, whats happening with joegame today?"
          '';
        };
      });
}
# In shell hook
# LD_LIBRARY_PATH=${
#   lib.makeLibraryPath [
#     libGL
#     SDL2
#     SDL2_image
#     SDL2_ttf
#     libffi
#     # node canvas
#     cairo
#     #pango
#     libjpeg
#     giflib
#     netsurf.libsvgtiny
#     libuuid
#   ]
# }:$LD_LIBRARY_PATH
