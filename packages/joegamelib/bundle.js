function clone(obj){
    return Object.assign({}, obj)
}

const baseBuild = {
    entryPoints: [
        './src/index.ts',
    ],
    format: 'iife',
    globalName: 'joegameLib',
    target: require('./browser-targets'),
    bundle: true,
    outfile: 'dist/joegame-lib.min.js',
    minify: true,
    define: {
        BASEURL: JSON.stringify('/joegame/'),
    }
}

const devBuild = Object.assign(clone(baseBuild),{
    minify: false,
    sourcemap: 'inline'
})

const succeedmsg =`
            ///////////////////////////////////////////////////////////////////
            //                     watch build succeeded                     //
            ///////////////////////////////////////////////////////////////////
`

const watchBuild = Object.assign(clone(devBuild), {
    watch: {
        onRebuild(error, result) {
            if (error) console.error('watch build failed:', error)
            else console.log(succeedmsg, result)
        },
    },
})

if(require.main === module){
    if(!(process.argv[2])){
        require('esbuild').build(baseBuild)
    }
}
