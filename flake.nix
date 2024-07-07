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
        pks = [ "noise" "sf3convert" ];

        collectedPks = builtins.foldl' (acc: name:
          let pkg = pkgs.callPackage ./packages/${name} { };
          in {
            packages.${name} = pkg;
            devShells.${name} = pkgs.mkShell { inputsFrom = [ pkg ]; };
          } // acc) { } pks;
      in rec {

        # packages.noise = pkgs.callPackage ./packages/noise { };
        # devShells.noise = pkgs.mkShell { inputsFrom = [ packages.noise ]; };

        packages.make = pkgs.gnumake;
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

        devShells.baseDev = pkgs.mkShell { buildInputs = baseDevInputs; };
        devShells.zigDev =
          pkgs.mkShell { buildInputs = [ zigpkgs.zigpkgs.master pkgs.zls ]; };
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
      } // collectedPks);
}
# In shell hook
