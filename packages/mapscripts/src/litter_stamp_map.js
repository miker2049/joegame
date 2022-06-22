const fs = require("fs/promises")
const path = require("path")
const assert = require("assert")
const { title } = require("process")

const inflate = require("./parse-tiled-zlib")

/*
 * Takes a number and a width, and returns a tuple
 * [x,y]
 * @param {number} n - the index
 * @param {number} w - the width
 * @returns number[]
 */

/*
 * Gets a json parsed object from a path
 * @param {string} path - the path
 * @returns {object}
 */
async function getObjectFromPath(path) {
    return JSON.parse(await fs.readFile(path, 'utf-8'))
}

function parseStamp(obj) {
    if (!obj.variations) return Error('stamp input error, no variations')
    let out = []
    obj.variations.forEach(variation => {
        let varObj = {}
        varObj.probability = variation.probability
        varObj.tileset = path.basename(variation.map.tilesets[0].source)
        varObj.width = variation.map.width
        varObj.data = inflate(variation.map.layers[0].data)
        out.push(varObj)
    });
    return out
}

function arrChoose(arr) {
    return arr[Math.floor(Math.random() * arr.length)]
}

function fixStampDataGidToMap(arr, newgid) {
    let out = []
    arr.forEach(tile => {
        //save flips
        const f = tile & 0xe0000000
        let clean = (tile & ~0xe0000000) - 1 //stamp firstgid is always one
        clean += newgid
        out.push(clean | f)
    });
    return out
}


function placeStampNoOverlap(layerData, layerDataWidth, stampData, stampWidth, length, stampRows, used, repeats) {
    const MAX_ITER = 100
    if (repeats > MAX_ITER) return undefined
    const choice = indexToCoords(Math.floor(Math.random() * length), layerDataWidth)
    let tmp = []

    for (let i = 0; i < stampData.length; i++) {
        let tile = stampData[i]
        const x = (i % stampWidth) + choice[0]
        const y = Math.floor(i / stampWidth) + choice[1]
        const proposedIndex = coordsToIndex(x, y, layerDataWidth)
        if (used.includes(proposedIndex) ||
            proposedIndex > layerData.length ||
            y > (layerData / layerDataWidth) - 1 ||
            x > layerDataWidth - 1) {
            repeats+=1
            return placeStampNoOverlap(layerData, layerDataWidth, stampData, stampWidth,
                length, stampRows, used, repeats)
        }
        tmp.push([proposedIndex, tile])
    }

    const beforeLength = layerData.length

    tmp.forEach(v => {
        layerData[v[0]] = v[1]
        used.push(v[0])
    })

    assert(beforeLength === layerData.length, "total data does not change");
    return layerData
}

function placeStamp(layerData, layerDataWidth, stampData, stampWidth, x, y) {
    stampData.forEach((tile, i) => {
        const mapx = (i % stampWidth) + x
        const mapy = Math.floor(i / stampWidth) + y
        const proposedIndex = coordsToIndex(mapx, mapy, layerDataWidth)
        layerData[proposedIndex] = tile
    });
    return layerData
}

/*
 * Credit: https://stackoverflow.com/a/55671924
 */
function weighted_random(arr) {
    let i;
    let weights = [];
    for (i = 0; i < arr.length; i++)
        //weights are the accumulation of previous weights
        weights[i] = arr[i].probability + (weights[i - 1] || 0);
    //the random number is from the final weight
    let random = Math.random() * weights[weights.length - 1];

    // keep moving i until it hits random
    for (i = 0; i < weights.length; i++)
        if (weights[i] > random)
            break;

    return arr[i];
}

function sprayOnLayer(map, layerData, layerDataWidth, stamps, n) {
    const length = layerData.length
    const stampPlaces = stamps.reduce((acc,v)=>{
        return v.data.length+acc
    },0)
    let repeats = (length/(stampPlaces/stamps.length)) * n
    console.log(length, stampPlaces,repeats)
    let used = []
    while (repeats > 0) {
        const stamp = weighted_random(stamps)
        const gid = map.tilesets.find(v => path.basename(v.source ?? "") == stamp.tileset).firstgid
        const stampData = fixStampDataGidToMap(stamp.data,gid)
        const stampWidth = stamp.width
        const stampRows = stampData / stampWidth
        const res = placeStampNoOverlap(layerData, layerDataWidth, stampData, stampWidth,
            length, stampRows, used, 0)
        if (!res) {
            console.log("too many times")
            return layerData;
        }
        layerData = res
        repeats--
    }
    return layerData
}




; (async () => {
    const map = await getObjectFromPath(process.argv[2])
    const layerName = process.argv[4]
    const n = process.argv[5]
    const outfile = process.argv[6]
    let stamps = parseStamp(await getObjectFromPath(process.argv[3]))
    let layerI = map.layers.findIndex(v => v.name == layerName)
    map.layers[layerI].data = sprayOnLayer(map,
                                           map.layers[layerI].data,
                                           map.layers[layerI].width,
                                           stamps, n, 0)

    await fs.writeFile(outfile, JSON.stringify(map), 'utf-8')
})()
