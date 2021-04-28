const chokidar = require('chokidar');
const liveServer = require('live-server');
const esbuild = require('esbuild');
(async () => {

  // Initialize watcher.
  const watcher = chokidar.watch(['./test/assets/*', './test/*.test.js', './src/**/*'], {
    // ignored: /(^|[\/\\])\../, // ignore dotfiles
    persistent: true
  });

  const result = await esbuild.build({
    entryPoints: ['test/index.test.ts'],
    target: [
      'chrome78',
      'firefox67',
      'safari13',
      'edge66',
    ],
    bundle: true,
    // watch: true,
    incremental: true,
    outfile: 'test/test-bundle.js',
    sourcemap: 'inline',
    loader: {
      '.png': 'dataurl',
      '.csv': 'text',
    },
  }).catch(err => console.error(err))

  console.log(result)
  console.log(`server running at localhost:8001`)
  // Call "stop" on the web server when you're done
  // server.stop()
  //

  // Add event listeners.
  watcher.on('change', (path, stats) => {
    console.log(`File ${path, stats} has been changed`)
    result.rebuild().then(msg => console.log(msg), err => console.error(err))
  })

  liveServer.start({
    root: 'test',
    port: 8001,
    open: false,
  });
})()
