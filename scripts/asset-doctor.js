const chalk = require('chalk');
const fs = require('fs')
const fsp = require('fs/promises')
const glob = require('glob')

const sectionLog = (str) => console.log(chalk.black.bgBlue(str))
function exists(path) {
    try {
        fs.accessSync(path)
        return true
    } catch (err) {
        // Do something
        return false
    }
}

/*
 * Compare convo manifest with actually existing files
 */

sectionLog("Compare convo manifest with actually existing files")

const mani = JSON.parse(fs.readFileSync('assets/tweet-convos/convo-manifest.json'))

let missing = []
mani.forEach(f => {
    const ex = exists('assets/tweet-convos/' + f)
    if (!ex) {
        console.log(chalk.red(f, 'is missing from the convos folder but present in the manifest'))
    }
});

function checkMapFileAssets (instr) {
    let parse = JSON.parse(instr)
    // console.log(parse.tilesets.images ?? undefined)
    parse.tilesets.forEach(item => {
        if(item.image){
            console.log(item.image)
        } else {
            item.tiles.forEach(ti => console.log(ti.image) );
        }
    });
}


glob("assets/maps/*.json", function(er,files) {

    Promise.all(
        files.map((item, i, arr) => {
            return fsp.readFile(item, {encoding: "utf-8"})
        })
    ).then((files) => {
            // console.log(f)
        files.forEach((it)=>checkMapFileAssets(it))
    }).then((f)=>{
        // console.log(f)
    })
})
