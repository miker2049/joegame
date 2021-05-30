
require('esbuild').build({
    entryPoints: [
        './src/index.ts',
    ],
    target: [
        'chrome78',
        'firefox67',
        'safari13',
        'edge66',
    ],
    bundle: true,
    // watch: true,
    outfile: 'dist/bundle/bundle.min.js',
    minify: true,
    loader: {
        '.png': 'dataurl',
        '.csv': 'text',
    },
})
