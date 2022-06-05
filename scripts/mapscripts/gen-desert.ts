import jimp from 'jimp';
import { applyCliffs } from './cliff-maker';
import { checkTiledLayerProperty, Grid, getSubArr, getReplacementSet, normalizeGrid, snapNormalGrid, consolidateGrids, iterateGrid, addChunk, DataGrid } from './mapscript-utils';
import { scanAlphaToGrid, getWangColorGrids, applyPixelWangs } from './png2tilemap';
import { TiledMap } from './TiledMap';


const WANGSIZE = 4
const CLIFFLAYERNAME = 'cliffs'
const CLIFFMAX = 3
const TRAINLAYERS = 3

    ; (async function () {
        if (process.argv.length < 4) return console.error(`run script with picture and stamp file`)
        // let img = await jimp.read("assets/maps/desert/meta-map-sm.png")
        const img = await jimp.read(process.argv[2])
        const stamps = await TiledMap.fromFile(process.argv[3])
        const worldWidth = WANGSIZE * img.bitmap.width
        const worldHeight = WANGSIZE * img.bitmap.height
        const finalMap = TiledMap.createEmpty(worldWidth, worldHeight, stamps.getConf())

        const colorGrids = getWangColorGrids(stamps)
        let colorLayerGrids = colorGrids.map(item =>
            applyPixelWangs(item[1], WANGSIZE, item[0], img));
        colorLayerGrids = consolidateGrids(colorLayerGrids, TRAINLAYERS)
        // finalMap.applyLgs(colorLayerGrids, "color")

        const alphaMap = scanAlphaToGrid(img)
        const normalAlpha = normalizeGrid(alphaMap)
        const altMap = snapNormalGrid(normalAlpha, CLIFFMAX, true)
        const cliffGridIndex = stamps.getLayers().find(l => l.name === CLIFFLAYERNAME).id

        let cliffstampGrid = stamps.lg[cliffGridIndex]
        const cliffRegion = checkTiledLayerProperty(stamps.getConf(), cliffGridIndex, CLIFFLAYERNAME + "-region")
        if (cliffRegion) {
            const pRegion = cliffRegion.split('-').map(i => parseInt(i))
            cliffstampGrid = getSubArr(pRegion[0], pRegion[1], pRegion[2], pRegion[3], cliffstampGrid)
        } else {
            throw Error("Is cliff region specified?")
        }
        const replacers = getReplacementSet(stamps, CLIFFLAYERNAME)
        const replacers2 = getReplacementSet(stamps, CLIFFLAYERNAME, 2)
        const replacers3 = getReplacementSet(stamps, CLIFFLAYERNAME, 3)
        const cliffLayerGrids: Grid[] = applyCliffs(cliffstampGrid, CLIFFLAYERNAME, altMap, [replacers, replacers2, replacers3])

        // Collapse/order our grids
        /*
         * At this point, there is two collections of Grids, "color" or terrain grids, and the cliffs,
         * the are both organized where the higher the index, the "higher" the layer.
         *
         * The allocation of tiles across layers changes for each altitude, so we iterate through the
         * altitude grid at the first level.  an altitude of 0 means there are no cliffs and the terrain
         * is at its lowest section.  Because the terrain is multiple layers itself, each possible cliff
         * area will have its own set of terrain layers. Assuming we do some other "compress" step later,
         * I will make empty grids of all possible layers. It should look like this if there is 3 cliffs max and
         * 3 colors:
         * 11 cliff
         * 10 color
         * 9 color
         * 8 color
         * 7 Cliff
         * 6 Color
         * 5 color
         * 4 color
         * 3 cliff
         * 2 color
         * 1 color
         * 0 color
         *
         * Both 0 tiles and first floor tiles can be the same. And colors are always one less than
         * "their" cliffs.
         *
         * If v is 2, the three terrains will go on 4,5,6 or +4,+5,+6, if its 3 its 8,9,10
         * so it seems like the calc is (alt*(colorGridlength+1))+colorGridIndex
         * for each cliff, the indexes go by 3,7,11,15 so ((colorGridLength)*(i+1))-1          (4*n)+3
         *
         */
        const finalGridCollection: Grid<number>[] = Array((cliffLayerGrids.length * colorLayerGrids.length) + cliffLayerGrids.length)
            .fill(0).map(_ => DataGrid.createEmpty(altMap.width * 4, altMap.height() * 4, 0))
        iterateGrid(altMap, (x, y, v) => {
            for (let i = 0; i < cliffLayerGrids.length; i += 1) {
                // const thisCliffIndex =  i === 0 ? colorLayerGrids.length :
                //     (colorLayerGrids.length * (i+1)) + 2

                const thisCliffIndex = ((colorLayerGrids.length + 1) * i) + (colorLayerGrids.length)
                addChunk(finalGridCollection[thisCliffIndex],
                    getSubArr(x * 4, y * 4, 4, 4, cliffLayerGrids[i]),
                    (x * 4)-0, y * 4, 0)
                for (let j = 0; j < colorLayerGrids.length; j += 1) {
                    const thisColorIndex = (v * (colorLayerGrids.length + 1)) + j
                    addChunk(finalGridCollection[thisColorIndex],
                        getSubArr(x * 4, (y * 4), 4, 4, colorLayerGrids[j]),
                        x * 4, (y * 4) - (v * 4), 0)
                    if (v > 1) {
                        // addChunk(finalGridCollection[thisColorIndex],
                        //     getSubArr(x * 4, y * 4, 4, 4, colorLayerGrids[j]),
                        //     x * 4, y * 4, 0)
                    }
                }
            }
        })

        // const cliffLayerGrids = applyCliffs(stamps.lg[cliffGridIndex], CLIFFLAYERNAME, altMap,[replacers,replacers2])
        // finalMap.applyLgs(finalGridCollection, "n")
        // finalMap.applyLgs(cliffLayerGrids, "f")
        // finalMap.applyLgs(cliffLayerGrids, "c")
        finalMap.applyLgs(finalGridCollection, "f")

        console.log(altMap.print())
        console.log("got here")
        await finalMap.write('assets/maps/desert/ttmap.json')
    })()
