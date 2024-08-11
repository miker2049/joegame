{
  description = "my project description";
  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/740a7a489a6b4e60accdfd1568bf9423ae867622";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.disko.url = "github:nix-community/disko";
  inputs.disko.inputs.nixpkgs.follows = "nixpkgs";
  outputs = { self, nixpkgs, flake-utils, poetry2nix, disko }:
    let
      mainout = flake-utils.lib.eachDefaultSystem (system:
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
              buildInputs = (with pkgs;
                [
                  nodejs_20
                  # package
                  nodePackages.typescript-language-server
                  nodePackages.prettier
                ] ++ package.nativeBuildInputs);
              shellHook = with pkgs; ''
                export CL_SOURCE_REGISTRY=$HOME/joegame/packages/world/:$HOME/joegame/packages/assets/:$HOME/joegame/packages/server/
              '';
              # shellHook = with pkgs; ''
              #   export NODE_PATH=${lib.makeLibraryPath [ package ]}:$NODE_PATH
              # '';
            };

          mkDefaultDevShell = package:
            pkgs.mkShell { inputsFrom = [ package ]; };

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

          extraPackages.joegame-server =
            (let sbcl' = (pkgs.sbcl.withPackages (p: [ packages.server ]));
            in pkgs.writeScriptBin "joegame-server" ''
              ${
                (pkgs.sbcl.withPackages (p: [ packages.server ]))
              }/bin/sbcl --eval '(load (sb-ext:posix-getenv \"ASDF\"))' --eval '(asdf:load-system :server)'
            '');

          packages = extraPackages // (mkPackages packages);
          devShells = extraShells // (mkDevShells packages);
          apps.joegame-server = {
            type = "app";
            program = "${packages.joegame-server}/bin/joegame-server";
          };

        });
      mkHetznerCloud = ip:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            disko.nixosModules.disko
            ./joegame1.nix
            self.nixosModules.caddy-prod
          ];
          specialArgs = { ip6addr = ip; };
        };
    in mainout // {
      nixosModules.caddy-prod = { config, pkgs, ... }: {
        config = {
          services.caddy = {
            email = "mike@joegame.xyz";
            acmeCA = "https://acme-v02.api.letsencrypt.org/directory";
            enable = true;
            # virtualHosts."localhost".extraConfig = ''
            #   respond "Hello, world!"
            # '';
            virtualHosts."joegame.xyz".extraConfig = ''

              encode gzip
              file_server
              root * ${mainout.packages."x86_64-linux".site}
            '';
          };
        };
      };
      nixosConfigurations.joegame1 = mkHetznerCloud "2a01:4ff:f0:2e3b::1/64";
    };
}
# In shell hook
