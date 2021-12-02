const fs = require("fs/promises")
const path = require("path")

const inflate = require("./parse-tiled-zlib")

/*
 * Takes a number and a width, and returns a tuple
 * [x,y]
 * @param {number} n - the index
 * @param {number} w - the width
 * @returns number[]
 */
function indexToCoords(n, w){
   return [
       n % w,
       Math.floor(n/w)
   ]
}

function coordsToIndex(x,y, width){
    return (y*width)+x
}

/*
 * Gets a json parsed object from a path
 * @param {string} path - the path
 * @returns {object}
 */
async function getObjectFromPath(path){
    return JSON.parse(await fs.readFile(path,'utf-8'))
}

function parseStamp(obj){
    if(!obj.variations) return Error('stamp input error, no variations')
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

function arrChoose(arr){
    return arr[Math.floor(Math.random()*arr.length)]
}

function fixStampGidToMap(arr,newgid){
    let out = []
    arr.forEach(tile =>{
        //save flips
        const f = tile & 0xe0000000
        let clean = (tile & ~0xe0000000) - 1 //stamp firstgid is always one
        clean += newgid
        out.push(clean | f)
    });
    return out
}


function placeStampNoOverlap(layerData, layerDataWidth, stampData, stampWidth, length, stampRows, used, repeats){
    const MAX_ITER = 10000
    if(repeats>MAX_ITER) return undefined

    const choice = indexToCoords(Math.floor(Math.random()*length), layerDataWidth)

    if(
        choice[0] + stampWidth > layerDataWidth
        || choice[1] + stampRows > layerData/layerDataWidth
    ){
        return placeStampNoOverlap(layerData, layerDataWidth, stampData, stampWidth,
                                   length, stampRows, used, repeats++)
    }
    stampData.forEach((tile, i) => {
        const x = (i%stampWidth) + choice[0]
        const y = Math.floor(i/stampWidth) + choice[1]
        const proposedIndex = coordsToIndex(x,y,layerDataWidth)
        if(used.includes(proposedIndex)){
            return placeStampNoOverlap(layerData, layerDataWidth, stampData, stampWidth,
                                       length, stampRows, used, repeats++)
        }
        layerData[proposedIndex] = tile

    });
    return layerData
}

function sprayOnLayer(layerData, layerDataWidth, stampData, stampWidth, n){
    const length = layerData.length
    let repeats = Math.floor(length * n)
    const stampRows = stampData/stampWidth
    let used = []

    while(repeats>0){
        const res = placeStampNoOverlap(layerData, layerDataWidth, stampData, stampWidth,
                                   length, stampRows, used, 0)
        if(!res){
            Error("too many times")
            break;
        }
        layerData = res
        repeats--
    }
    return layerData
}




;(async ()=>{
    const map = await getObjectFromPath(process.argv[2])
    const layerName = process.argv[4]
    const n = process.argv[5]
    const outfile = process.argv[6]
    let stamp = await getObjectFromPath(process.argv[3])
    stamp = parseStamp(stamp)
    stamp =  arrChoose(stamp)
    const gid = map.tilesets.find(v=> path.basename(v.source??"")==stamp.tileset).firstgid
    let layer = map.layers.find(v=>v.data)
    layer.data = new Array(layer.data.length).fill(0)
    layer.name = layerName
    stamp.data = fixStampGidToMap(stamp.data, gid)
    layer.data = sprayOnLayer(layer.data, layer.width,stamp.data,stamp.width,n,0)
    layer.id = map.layers.length  +1
    map.layers.push(layer)
    await fs.writeFile(outfile,JSON.stringify(map),'utf-8')
    // console.log(layer)
})()
