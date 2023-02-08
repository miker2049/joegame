const succeedmsg = `            ///////////////////////////////////////////////////////////////////
            //                     watch build succeeded                     //
            ///////////////////////////////////////////////////////////////////
`
require('esbuild').build({
  entryPoints: ['./src/index.dev.ts'],
  bundle: true,
  outfile: 'dist/joegame-lib.min.js',
  format: 'iife',
  globalName: 'joegameLib',
  target: require('./browser-targets'),
  minify: false,
  sourcemap: 'inline',
  watch: {
    onRebuild(error, result) {
      if (error) console.error('watch build failed:', error)
      else console.log(succeedmsg, result)
    }
  },
  define: {
    BASEURL: JSON.stringify('/')
  }
})
