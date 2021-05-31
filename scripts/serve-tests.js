require('esbuild').serve({
    servedir: '.',
}, {
    entryPoints: [
        './test/main.test.ts',
        './test/utils.test.ts',
        './test/post-loading.test.ts',
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
