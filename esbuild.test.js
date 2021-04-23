const {watch} = require('chokidar')
const { serve, build } = require('esbuild')
const glob = require('tiny-glob');


const baseConfig = {
  target: [
    'chrome78',
    'firefox67',
    'safari13',
    'edge66',
  ],
  bundle: true,
  sourcemap: true,
  loader: {
    '.png': 'dataurl',
    '.csv': 'text',
  },
}

const testconfig = async ()=>{
  
  const files = await glob('test/*.test.ts')
  return Object.assign({
  entryPoints :  files,
  outdir: "test"
}, baseConfig)}

const soundconfig = Object.assign({
  entryPoints : ['test/soundDebug.ts'],
  outfile: "test/sounddebug.js"
}, baseConfig)

const buildBundles = async ()=>{
  
  const timerStart = Date.now();
  await Promise.all([

  build(await testconfig()),
  build(soundconfig)

  ]).catch(e=>console.error(e))
  const timerEnd = Date.now();
  console.log(`Built in ${timerEnd - timerStart}ms.`, true); 
}

// watch('src/**/*').on('all',()=>buildBundles())
watch('test/*.test.ts').on('all',()=>buildBundles())