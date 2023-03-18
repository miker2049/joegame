{
  description = "my project description";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
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
            gnuplot
            nodejs-18_x
            lilypond
            sf3convert
            nodePackages.pnpm
            nodePackages.prettier
            nodePackages.typescript-language-server
            nodePackages.typescript
            # (deno.overrideAttrs (old: rec { version = "1.30.3"; }))
            pkgs-unstable.deno
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
            zlib
            cmakeCurses
            pkg-config
          ];
          shellHook = with pkgs; ''
            LD_LIBRARY_PATH=${
              lib.makeLibraryPath [
                libGL
                SDL2
                zlib
                SDL2_image
                SDL2_ttf
                libffi
                sqlite
                # node canvas
                cairo
                #pango
                libjpeg
                giflib
                netsurf.libsvgtiny
                libuuid
              ]
            }:$LD_LIBRARY_PATH

            echo "Welcome, mike, whats happening with joegame today?"
          '';
        };
      });
}
# In shell hook
