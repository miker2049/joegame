const chalk = require('chalk')
const fs = require('fs/promises')
const request = require('request');
const JSZip = require("jszip");

const EMOJIURL = "https://github.com/hfg-gmuend/openmoji/releases/download/13.0.0/openmoji-72x72-color.zip"
const EMOJIPATH = "assets/images/emoji"

// JSZip.loadAsync(body).then(function (zip) {
//   return zip.file("content.txt").async("string");
// }).then(function (text) {
//   console.log(text);
// });
function reqEmoji() {
    return new Promise((res, rej) => {
        request({
            method: "GET",
            url: EMOJIURL,
            encoding: null // <- this one is important !
        }, function(error, response, body) {
            if (error || response.statusCode !== 200) {
                rej(error)
                return;
            }
            res(body)
        });
    })
}
(async function() {
    const filesExisting = await fs.readdir(EMOJIPATH)
    if (filesExisting.length > 2000) {
        console.log('we alread have emojis! canceling download')
    } else {

        const body = await reqEmoji()
        const zip = await JSZip.loadAsync(body)
        await Promise.all(Object.keys(zip.files).map((item, _i, _arr) => {
            zip.file(item).async('uint8array').then(buff => {
                console.log(`writing ${chalk.red(EMOJIPATH + "/" + item)}`)
                return fs.writeFile(`${EMOJIPATH}/${item}`, buff)
            })
        }))
        console.log('done')
    }
})()
