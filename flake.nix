{
  description = "my project description";
  inputs.zig.url = "github:mitchellh/zig-overlay";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, poetry2nix, zig }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        zigpkgs = import nixpkgs {
          inherit system;
          overlays = [ zig.overlays.default ];
        };
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
      in rec {

        packages.joegame-noise-libs = joegame-noise-libs;
        packages.make = pkgs.gnumake;
        packages.sf3convert = sf3convert;
        packages.sbcl-env = sbcl-env;
        baseDevInputs = with pkgs; [
          # guile_3_0
          nodejs-18_x
          sf3convert
          # python3Packages.spacy
          nodePackages.pnpm
          nodePackages.prettier
          nodePackages.typescript-language-server
          nodePackages.vscode-json-languageserver
          nodePackages.typescript
        ];
        lispDevInputs = with pkgs; [
          # guile_3_0
          nodejs-18_x
          # python3Packages.spacy
          nodePackages.pnpm
          nodePackages.prettier
          nodePackages.typescript-language-server
          nodePackages.vscode-json-languageserver
          nodePackages.typescript
        ];

        devShells.baseDev = pkgs.mkShell { buildInputs = baseDevInputs; };
        devShells.zigDev = pkgs.mkShell {

          buildInputs = [ zigpkgs.zigpkgs.master pkgs.zls ];
        };
        devShells.wrDev = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              libpng
              ncurses
              wasmtime
              sbcl
              (python3.withPackages (ps: with ps; [ matplotlib ]))
            ] ++ devShells.zigDev.buildInputs;
        };
        devShell = devShells.baseDev;
        devShells.fullDev = pkgs.mkShell {
          venvDir = "./.venv";
          buildInputs = (with pkgs; [
            roswell
            # (deno.overrideAttrs (old: rec { version = "1.30.3"; }))
            deno
            djlint
            (python3.withPackages (ps:
              with ps; [
                matplotlib
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
                # (buildPythonPackage rec {
                #   pname = "webvtt_py";
                #   version = "0.4.6";
                #   format = "wheel";
                #   src = fetchPypi rec {
                #     inherit pname version format;
                #     sha256 =
                #       "sha256-XPnaKow0vHidtZk3e+h7Iot+RzTGKVl+3SeoBU4ASlc=";
                #     dist = python;
                #     python = "py3";
                #   };
                #   doCheck = false;
                #   propagatedBuildInputs = with ps;
                #     [
                #       # Specify dependencies
                #     ];
                # })
                beautifulsoup4
                numpy
                python-lsp-server
                pip
                scipy
                pysrt
                youtube-dl
              ]))
            sqlite
            sbcl
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
            c2ffi
            openssl
            roswell
            tk
            zstd
            yt-dlp
            kondo
            jdk21
            yarn # for shadow-cljs
          ]) ++ baseDevInputs;
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
                tk
                zstd
              ]
            }:$LD_LIBRARY_PATH

            echo "Welcome, mike, whats happening with joegame today?"
          '';
        };
      });
}
# In shell hook
