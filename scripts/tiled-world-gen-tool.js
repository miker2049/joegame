/*
 * This tool helps generate tiled worlds on the command line. The main
 * procedure entails taking a input tiled map (JSON format) and and generating
 * a world by copying the file
 *
 *
 *
 */
const { readFile, mkdir, writeFile } = require("fs/promises")

const path = require('path')

const COLS = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T']

async function createEmptyMap() {
    return JSON.parse(await readFile("assets/maps/empty.json"))
}

function createEmptyWorld() {
    return {
        maps: [],
        onlyShowAdjacentMaps: false,
        type: "world"
    }

}

function createMapWorldEntry(fileName, height, width, x, y) {
    return {
        fileName,
        height,
        width,
        x,
        y,
    }

}


// createEmptyMap().then(d=>console.log(d))
console.log(process.argv[2])

async function createFileFromTemplate(filepath, template, override = {}) {
    await writeFile(filepath, JSON.stringify(Object.assign(template, override)), {
        encoding: 'utf-8'
    })
}

async function genWorld(template,
    basename,
    n,
    basepath,
    offsetX = 0,
    offsetY = 0,
    world = createEmptyWorld()) {

    const width = template.width * template.tileheight
    const height = template.height * template.tileheight
    const thesemaps = []
    for (let row = 0; row < n; row++) {
        for (let col = 0; col < n; col++) {
            const name = basename + '_' + COLS[col] + row.toString() + '.json'
            const entry =  createMapWorldEntry(name, height, width,
                offsetX + (width * row),
                offsetY + (height * col))
            world.maps.push(entry)
            thesemaps.push(entry)
        }
    }
    await Promise.all(thesemaps.map((item) => createFileFromTemplate(basepath + item.fileName, template)))
    return world
}

/*
 * gen-tool generate {template} {n}
 *   takes a template and generates n^2 maps and a tiled world
 *
 * gen-tool add {template} {worldfile} {n} {x} {y}
 *   takes a template and a world file and adds to a world in the same manner
 *   as generate.  Needs to have its offsets manually added
 *
 */
async function main() {
    const command = process.argv[2]
    if (command == "generate") {
        const template = JSON.parse(await readFile(process.argv[3]))
        const basename = path.basename(process.argv[3], '.json')
        const basepath = path.dirname(process.argv[3]) + '/'
        const n = process.argv[4]
        const world = await genWorld(template, basename, n, basepath)
        await writeFile(basepath + basename + '.world', JSON.stringify(world))
    } else if (command == "add") {
        const template = JSON.parse(await readFile(process.argv[3]))
        const worldfile = JSON.parse(await readFile(process.argv[4]))
        const basename = path.basename(process.argv[3], '.json')
        const basepath = path.dirname(process.argv[3]) + '/'
        const n = parseInt(process.argv[5])
        const x = parseInt(process.argv[6])
        const y = parseInt(process.argv[7])
        const world = await genWorld(template, basename, n, basepath, x, y, worldfile)
        await writeFile(process.argv[4], JSON.stringify(world))
    }
}
main()
