const pako = require("pako")
// Inflate (simple wrapper can throw exception on broken stream)
//
//




/*
 * Takes one base64 encoded string, decodes it
 * and inflates the resulting zlib compressed data
 * into a Uint8Array.
 */
function inflateZlibBase64String(input) {
    let data
    try {
        const  b =  Buffer.from(input, 'base64')
        console.log(b)
        data = pako.inflate(b);
        // data = pako.inflate(Buffer.from(input, 'base64'));
    } catch (err) {
        return Error(err, input)
    }
    return new Uint32Array(data.buffer)
    // return data
}


// if(process.argv.length > 0) {
// const o = inflateZlibBase64String(process.argv[2])
// console.log(o)
// }

module.exports = inflateZlibBase64String
