{
  description = "my project description";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs-18_x
            nodejs-18_x.pkgs.pnpm
            nodejs-18_x.pkgs.typescript-language-server
            nodejs-18_x.pkgs.typescript
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
          ];
          shellHook = ''
            LD_LIBRARY_PATH=${
              pkgs.lib.makeLibraryPath [
                pkgs.libGL
                pkgs.SDL2
                pkgs.SDL2_image
                pkgs.SDL2_ttf
                pkgs.libffi
              ]
            }:$LD_LIBRARY_PATH
          '';
        };
      });
}
