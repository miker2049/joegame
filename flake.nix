{
  description = "my project description";
  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/0c53b6b8c2a3e46c68e04417e247bba660689c9d";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, poetry2nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        zigpkgs = import nixpkgs {
          inherit system;
          overlays = [ ];
        };
        mkLispDevShell = package:
          pkgs.mkShell {
            buildInputs = (with pkgs;
              [ (sbcl.withPackages (ps: package.lispLibs)) ]
              ++ package.nativeLibs);
            shellHook = with pkgs; ''
              export CL_SOURCE_REGISTRY=$HOME/joegame/packages/world/:$HOME/joegame/packages/assets/
              export LD_LIBRARY_PATH=${
                lib.makeLibraryPath package.nativeLibs
              }:$LD_LIBRARY_PATH
            '';
          };
      in rec {

        packages.assets = pkgs.callPackage ./packages/assets { };
        devShells.assets = pkgs.mkShell { inputsFrom = [ packages.assets ]; };

        packages.noise = pkgs.callPackage ./packages/noise { };
        devShells.noise = pkgs.mkShell { inputsFrom = [ packages.noise ]; };

        packages.sf3convert = pkgs.callPackage ./packages/sf3convert { };
        devShells.sf3convert =
          pkgs.mkShell { inputsFrom = [ packages.sf3convert ]; };

        packages.clackup = pkgs.callPackage ./packages/clackup {
          sbcl = (pkgs.sbcl.withPackages
            (ps: [ packages.server ps.split-sequence ]));
        };

        devShells.clackup = pkgs.mkShell { inputsFrom = [ packages.clackup ]; };

        packages.world = pkgs.callPackage ./packages/world { ps = packages; };
        devShells.world = mkLispDevShell packages.world;
        # devShells.world = pkgs.mkShell {
        #   buildInputs = (with pkgs;
        #     [
        #       (sbcl.withPackages
        #         (ps: [ packages.assets ] ++ packages.world.lispLibs))
        #     ] ++ packages.world.nativeLibs);
        #   shellHook = with pkgs; ''
        #     export CL_SOURCE_REGISTRY=$HOME/joegame/packages/world/:$HOME/joegame/packages/assets/
        #     export LD_LIBRARY_PATH=${
        #       lib.makeLibraryPath packages.world.nativeLibs
        #     }:$LD_LIBRARY_PATH
        #   '';
        # };

        packages.server = pkgs.callPackage ./packages/server { ps = packages; };
        devShells.server = pkgs.mkShell { inputsFrom = [ packages.server ]; };

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
        devShells.pythonDev = pkgs.mkShell {
          venvDir = "./.venv";
          buildInputs = (with pkgs;
            [
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
            ]);
          postVenvCreation = ''
            unset SOURCE_DATE_EPOCH
            pip install -r requirements.txt
          '';
        };
      });
}
# In shell hook
