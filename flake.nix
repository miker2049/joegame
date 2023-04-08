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
          venvDir = "./.venv";
          buildInputs = with pkgs; [

            gnuplot
            nodejs-18_x
            lilypond
            sf3convert
            python3Packages.venvShellHook
            python3Packages.spacy
            glibc
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
            openjdk8
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
                libGL
                SDL2
                zlib
                SDL2_image
                SDL2_ttf
                libffi
                sqlite
                # node canvas
                cairo
                glibc
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
