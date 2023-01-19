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

            # node canvas
            cairo
            pango
            libjpeg
            giflib
            netsurf.libsvgtiny
            libuuid
          ];
          shellHook = with pkgs; ''
            LD_LIBRARY_PATH=${
              lib.makeLibraryPath [
                libGL
                SDL2
                SDL2_image
                SDL2_ttf
                libffi
                # node canvas
                cairo
                #pango
                libjpeg
                giflib
                netsurf.libsvgtiny
                libuuid
              ]
            }:$LD_LIBRARY_PATH
          '';
        };
      });
}
