const succeedmsg =
    `            ///////////////////////////////////////////////////////////////////
            //                     watch build succeeded                     //
            ///////////////////////////////////////////////////////////////////
`
require('esbuild').build({
    entryPoints: [
        './src/index.ts',
    ],
    bundle: true,
    outfile: 'bundle/joegame-lib.min.js',
    format: 'iife',
    globalName: 'joegameLib',
    target: require('../browser-targets'),
    minify: false,
    watch: {
        onRebuild(error, result) {
            if (error) console.error('watch build failed:', error)
            else console.log(succeedmsg, result)
        },
    },
})
