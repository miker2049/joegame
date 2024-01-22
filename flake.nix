{
  description = "my project description";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, poetry2nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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
        joegame-noise-libs = pkgs.stdenv.mkDerivation rec {
          pname = "joegame-noise-libs";
          version = "1.0";

          src = ./packages/noise;

          buildPhase = ''
            make libsimplex.so
            make libspooky.so
          '';

          installPhase = ''
            mkdir -p $out/lib
            cp ./libspooky.so $out/lib/libspooky.so
            cp ./libsimplex.so $out/lib/libsimplex.so
          '';
        };
        sbcl-env = pkgs.stdenv.mkDerivation rec {
          pname = "joegame-sbcl-env";
          version = "1.0";

          buildInputs = with pkgs.lispPackages; [
            pkgs.sbcl
            sqlite
            alexandria
            cl-async
            # colored
            bordeaux-threads
            blackbird
            jonathan
          ];
        };
      in {

        packages.joegame-noise-libs = joegame-noise-libs;
        packages.make = pkgs.gnumake;
        packages.sf3convert = sf3convert;
        packages.sbcl-env = sbcl-env;
        devShell = pkgs.mkShell {
          venvDir = "./.venv";
          buildInputs = with pkgs; [
            # guile_3_0
            nodejs-18_x
            sf3convert
            # python3Packages.spacy
            nodePackages.pnpm
            nodePackages.prettier
            nodePackages.typescript-language-server
            nodePackages.typescript
            # (deno.overrideAttrs (old: rec { version = "1.30.3"; }))
            deno
            djlint
            (python3.withPackages (ps:
              with ps; [
                pillow
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
                beautifulsoup4
                numpy
                python-lsp-server
                pip
                scipy
                pysrt
              ]))
            sqlite
            sbcl
            parallel
            libffi
            emscripten
            clang
            libjpeg
            libpng
            giflib
            netsurf.libsvgtiny
            libuuid
            imagemagick
            gnumake
            zlib
            cmakeCurses
            pkg-config
            libuv
            libffi
            gobject-introspection
            glib
            c2ffi
            openssl
            roswell
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
                zlib
                libffi
                sqlite
                gobject-introspection
                glib
                # node canvas
                #pango
                libjpeg
                libpng
                giflib
                libuuid
                libuv
                gtk4
                openssl
                joegame-noise-libs
                xxHash
                imagemagick
              ]
            }:$LD_LIBRARY_PATH

            echo "Welcome, mike, whats happening with joegame today?"
          '';
        };
      });
}
# In shell hook
