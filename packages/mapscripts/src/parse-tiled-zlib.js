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
        data = pako.inflate(b);
    } catch (err) {
        return Error(err, input)
    }
    return new Uint32Array(data.buffer)
}



module.exports = inflateZlibBase64String
