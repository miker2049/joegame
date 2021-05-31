require('esbuild').build({
    entryPoints: [
        './src/index.ts',
    ],
    target: require('../browser-targets'),
    watch: true,
    bundle: true,
    outfile: 'public/joegame-lib.min.js',
    minify: true
})
