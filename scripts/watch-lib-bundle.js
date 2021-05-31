console.log('hey')
const targets = require('../browser-targets')
console.log(targets)
require('esbuild').build({
    entryPoints: [
        './src/index.ts',
    ],
    target: targets,
    bundle: true,
    outfile: 'bundle/joegame-lib.min.js',
    minify: true,
    watch: {
        onRebuild(error, result) {
            if (error) console.error('watch build failed:', error)
            else console.log('watch build succeeded:', result)
        },
    },
})
