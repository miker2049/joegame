const chalk = require('chalk');
const fs = require('fs')

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

const mani = JSON.parse(fs.readFileSync('assets/convos/convo-manifest.json'))

let missing = []
mani.forEach(f => {
    const ex = exists('assets/convos/' + f)
    if (!ex) {
        console.log(chalk.red(f, 'is missing from the convos folder but present in the manifest'))
    }
});

