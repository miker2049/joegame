const { serve, build } = require('esbuild')
const glob = require('tiny-glob')

const buildTests = (entries) => build({
  entryPoints: entries,
  target: [
    'chrome78',
    'firefox67',
    'safari13',
    'edge66',
  ],
  bundle: true,
  // watch: true,
  outdir: 'test',
  external: ["fs"],
  sourcemap: true,
  loader: {
    '.png': 'dataurl',
    '.csv': 'text',
  },
})

const serveTests = (entries) => serve({
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
  outdir: 'test',
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

glob('test/*.test.ts').then((entries) => buildTests(entries))
