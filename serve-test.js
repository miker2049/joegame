require('esbuild').serve({
  servedir: 'test',
}, {
  entryPoints: [
    './test/main.test.js',
    './test/post-loading.test.js',
  ],
  target: [
    'chrome78',
    'firefox67',
    'safari13',
    'edge66',
  ],
  bundle: true,
  // watch: true,
  incremental: true,
  outdir: 'test/dist',
  sourcemap: 'inline',
  loader: {
    '.png': 'dataurl',
    '.csv': 'text',
  },
}).then(server => {
  // Call "stop" on the web server when you're done
  console.log(server)
  server.onRequest = (req) => console.log(req)
  // server.stop()
})
