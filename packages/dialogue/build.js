
require('esbuild').build({
  entryPoints: ['src/bondage.js', 'src/runner.js'],
  bundle: true,
  sourcemap: true,
  outdir: 'dist',
  platform: 'browser',
})
