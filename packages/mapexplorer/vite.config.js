import { resolve } from "path";
import { defineConfig } from "vite";

export default defineConfig({
    build: {
        lib: {
            // Could also be a dictionary or array of multiple entry points
            entry: resolve(__dirname, "lib/main.ts"),
            name: "mapexplorer",
            // the proper extensions will be added
            fileName: "mapexplorer",
            format: "es",
        },
        output: { format: "es" },
        rollupOptions: {
            input: {
                world: resolve(__dirname, "index.html"),
                maptest: resolve(__dirname, "maptest.html"),
            },
            output: {
                inlineDynamicImports: false,
                format: "es",
            },

            // make sure to externalize deps that shouldn't be bundled
            // into your library
            // external: ['vue'],
            // output: {
            //   // Provide global variables to use in the UMD build
            //   // for externalized deps
            //   globals: {
            //     vue: 'Vue',
            //   },
            // },
        },
    },
});
