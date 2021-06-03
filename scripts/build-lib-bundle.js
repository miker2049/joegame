require('esbuild').build({
    entryPoints: [
        './src/index.ts',
    ],
    format: 'iife',
    globalName: 'joegameLib',
    target: require('../browser-targets'),
    bundle: true,
    outfile: 'bundle/joegame-lib.min.js',
    minify: true
})
