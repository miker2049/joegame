require('esbuild').serve({
    servedir: '.',
}, {
    entryPoints: [
        './test/tests/main.test.ts',
        './test/tests/utils.test.ts',
        './test/tests/post-loading.test.ts',
    ],
    target: require('../browser-targets'),
    bundle: true,
    outdir: 'test/dist',
    loader: {
        '.png': 'dataurl',
        '.csv': 'text',
    },
    sourcemap: 'inline',
}).then(server => {
    console.log(server)
    server.onRequest = (req) => console.log(req)
    require('open')(`http://${server.host}:${server.port}/test/`)
})
