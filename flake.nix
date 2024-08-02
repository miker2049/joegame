{
  description = "my project description";
  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/0c53b6b8c2a3e46c68e04417e247bba660689c9d";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, poetry2nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        packsConfig = import ./packages.nix;
        packs = packsConfig.packs;

        mkLispDevShell = package:
          pkgs.mkShell {
            buildInputs = (with pkgs;
              [ (sbcl.withPackages (ps: package.lispLibs)) ]
              ++ package.nativeLibs);
            shellHook = with pkgs; ''
              export CL_SOURCE_REGISTRY=$HOME/joegame/packages/world/:$HOME/joegame/packages/assets/:$HOME/joegame/packages/server/
              export LD_LIBRARY_PATH=${
                lib.makeLibraryPath package.nativeLibs
                # map (it: (lib.makeLibraryPath it) + ":") package.nativeLibs
              }:$LD_LIBRARY_PATH
            '';
          };

        mkNodeDevShell = package:
          pkgs.mkShell {
            buildInputs = (with pkgs; [
              nodejs_20
              # package
              nodePackages.typescript-language-server
              nodePackages.prettier
            ]);
            shellHook = with pkgs; ''
              # export NODE_PATH=${lib.makeLibraryPath package}:$NODE_PATH
            '';
          };

        mkDefaultDevShell = package: pkgs.mkShell { inputsFrom = [ package ]; };

        mkPackages = packages:
          builtins.listToAttrs (map (pack: {
            name = pack.name;
            value = pkgs.callPackage ./packages/${pack.name} {
              joegamepkgs = packages;
            };
          }) packs);

        mkDevShells = packages:
          builtins.listToAttrs (map (pack: {
            name = pack.name;
            value = if pack.type == "lisp" then
              mkLispDevShell packages.${pack.name}
            else if pack.type == "js" then
              mkNodeDevShell packages.${pack.name}
            else
              mkDefaultDevShell packages.${pack.name};
          }) packs);
      in rec {

        extraShells.clackup =
          pkgs.mkShell { inputsFrom = [ packages.clackup ]; };
        extraPackages.clackup = pkgs.callPackage ./packages/clackup {
          sbcl = (pkgs.sbcl.withPackages
            (ps: [ packages.server ps.split-sequence ]));
        };

        packages = extraPackages // (mkPackages packages);
        devShells = extraShells // (mkDevShells packages);

      });
}
# In shell hook
