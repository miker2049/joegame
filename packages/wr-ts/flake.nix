{
  inputs = {
    joegame.url = "../../";
    # nixpkgs.follows = "joegame";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    # flake-utils.follows = "joegame";
  };
  outputs = { self, joegame, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        bp = import joegame.baseDevPkgs pkgs;
      in { devShell = pkgs.mkShell { buildInputs = bp; }; });
}
