let { serve, build } = require('esbuild')
const glob = require('tiny-glob');
(async function() {
  let entries = await glob('test/*.test.ts')
  await build()
  serve({
    servedir: 'test',
  }, {
    entryPoints: entries,
    target: [
      'chrome78',
      'firefox67',
      'safari13',
      'edge66',
    ],
    bundle: true,
    // watch: true,
    outfile: 'test/tests.js',
    external: ["fs"],
    sourcemap: true,
    loader: {
      '.png': 'dataurl',
      '.csv': 'text',
    },
  }).then(server => {
    console.log(server)
    // Call "stop" on the web server when you're done
    // server.stop()
  }).catch(err => console.error(err))
})();
