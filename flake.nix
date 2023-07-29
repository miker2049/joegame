{
  description = "my project description";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";
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
        rraylib = pkgs.raylib.overrideAttrs (finalAttrs: previousAttrs: rec {
          version = "4.5.0";
          patches = [ ];
          src = pkgs.fetchFromGitHub {
            owner = "raysan5";
            repo = previousAttrs.pname;
            rev = version;
            sha256 = "sha256-Uqqzq5shDp0AgSBT5waHBNUkEu0LRj70SNOlR5R2yAM=";
          };
        });
        raygui = pkgs.stdenv.mkDerivation rec {
          pname = "raygui";
          version = "3.6";

          src = pkgs.fetchFromGitHub {
            owner = "raysan5";
            repo = pname;
            rev = version;
            sha256 = "sha256-pd7V2SHRnyMOPP3HL8uS3eJXybUGL+CDnGeWt21n3/g=";
          };

          buildInputs = with pkgs; [
            mesa
            glfw
            xorg.libXi
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXinerama
            rraylib
          ];
          propagatedBuildInputs = [ pkgs.libGLU pkgs.xorg.libX11 ];

          patches = [ ];

          installPhase = ''
            mkdir -p $out/lib
            mkdir -p $out/include
            cp $src/src/raygui.h $out/raygui.c
            cp $src/src/raygui.h $out/include/raygui.h
            gcc -o $out/lib/libraygui.so $out/raygui.c -shared -fpic -DRAYGUI_IMPLEMENTATION -lraylib -lGL -lm -lpthread -ldl -lrt -lX11
            rm $out/raygui.c
          '';
          meta = with pkgs.lib; {
            description =
              "A simple and easy-to-use library to enjoy videogames programming";
            homepage = "https://www.raylib.com/";
            license = licenses.zlib;
            maintainers = with maintainers; [ adamlwgriffiths ];
            platforms = platforms.linux;
            changelog =
              "https://github.com/raysan5/raylib/blob/${version}/CHANGELOG";
          };
        };
      in {
        packages.emacss = emacss;
        packages.make = pkgs.gnumake;
        packages.sf3convert = sf3convert;
        devShell = pkgs.mkShell {
          venvDir = "./.venv";
          buildInputs = with pkgs; [
            ncurses
            guile_3_0
            gnuplot
            nodejs-18_x
            lilypond
            sf3convert
            python3Packages.venvShellHook
            python3Packages.spacy
            nodePackages.pnpm
            nodePackages.prettier
            nodePackages.typescript-language-server
            nodePackages.typescript
            # (deno.overrideAttrs (old: rec { version = "1.30.3"; }))
            pkgs-unstable.deno
            (python3.withPackages (ps:
              with ps; [
                nltk
                scikit-learn
                stanza
                lxml
                (buildPythonPackage rec {
                  pname = "EbookLib";
                  version = "0.18";
                  src = fetchPypi {
                    inherit pname version;
                    sha256 =
                      "sha256-OFYmQ6e8lNm/VumTC0kn5Ok7XR0JF/aXpkVNtaHBpTM=";
                  };
                  doCheck = false;
                  propagatedBuildInputs = with ps; [
                    # Specify dependencies
                    six
                    lxml
                  ];
                })
                # (buildPythonPackage rec {
                #   pname = "textblob";
                #   version = "0.7.0";
                #   src = fetchGit {
                #     # inherit pname version;
                #     url = "https://github.com/sloria/textblob";
                #     rev = "99450649bc8c3bf92ea33c94a1b7d7d65c8317c4";
                #     # sha256 =
                #     #   "sha256-rW25UCzQQvuasbUKgF6hZL/8YEDie4NUFqRhJ5HpguM=";
                #   };
                #   doCheck = false;
                #   propagatedBuildInputs = [
                #     # Specify dependencies
                #     pkgs.python3Packages.pyyaml
                #     pkgs.python3Packages.nltk
                #   ];
                # })
                beautifulsoup4
                numpy
                python-lsp-server
                spacy
                pip
                scipy
                pysrt
              ]))
            #t
            sqlite
            sbcl
            SDL2
            parallel
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
            cairo
            pango
            libjpeg
            libpng
            giflib
            netsurf.libsvgtiny
            libuuid
            imagemagick
            rclone
            gnumake
            zlib
            cmakeCurses
            pkg-config
            openjdk8
            tk
            libuv
            libffi
            gobject-introspection
            glib
            gtk4
            rraylib
            raygui
            c2ffi
            doxygen
            openssl
          ];
          postVenvCreation = ''
            unset SOURCE_DATE_EPOCH
            pip install -r requirements.txt
          '';
          shellHook = with pkgs; ''
            # allow pip to install wheels
            unset SOURCE_DATE_EPOCH
            LD_LIBRARY_PATH=${
              lib.makeLibraryPath [
                c2ffi
                libGL
                SDL2
                zlib
                SDL2_image
                SDL2_ttf
                libffi
                sqlite
                gobject-introspection
                glib
                raygui
                # node canvas
                cairo
                glibc
                #pango
                libjpeg
                libpng
                giflib
                netsurf.libsvgtiny
                libuuid
                tk
                libuv
                gtk4
                rraylib
                ncurses
                openssl
              ]
            }:$LD_LIBRARY_PATH

            export RAYLIB_H=${rraylib}/include/raylib.h
            export RAYGUI_H=${raygui}/include/raygui.h

            echo "Welcome, mike, whats happening with joegame today?"
          '';
        };
      });
}
# In shell hook
