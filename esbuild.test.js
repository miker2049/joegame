const liveServer = require('live-server');
require('esbuild').build({
  entryPoints: ['test/index.test.ts'],
  target: [
    'chrome78',
    'firefox67',
    'safari13',
    'edge66',
  ],
  bundle: true,
  watch: true,
  incremental: true,
  outfile: 'test/test-bundle.js',
  external: ["fs"],
  sourcemap: true,
  loader: {
    '.png': 'dataurl',
    '.csv': 'text',
  },
}).then(result => {
  console.log(result)
  console.log(`server running at localhost:8080`)
  // Call "stop" on the web server when you're done
  // server.stop()
  liveServer.start({
    root: 'test',
    port: 8001,
    open: false,
  });
}).catch(err => console.error(err))
