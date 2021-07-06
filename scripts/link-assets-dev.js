const fs = require('fs');

console.log('start it')
try {

    fs.lstatSync('public/assets')
} catch (err) {

    fs.symlinkSync('../assets', 'public/assets')
    console.log('synced it')
}
console.log('finished it')
