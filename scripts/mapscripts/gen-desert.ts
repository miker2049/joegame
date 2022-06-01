import jimp from 'jimp';
import { applyCliffs } from './cliff-maker';
import { checkTiledLayerProperty, getSubArr, getReplacementSet, normalizeGrid, snapNormalGrid } from './mapscript-utils';
import { scanAlphaToGrid, getWangColorGrids, applyPixelWangs } from './png2tilemap';
import { TiledMap } from './TiledMap';


const WANGSIZE = 4
const CLIFFLAYERNAME = 'cliffs'

    ; (async function () {
        if (process.argv.length < 4) return console.error(`run script with picture and stamp file`)
        // let img = await jimp.read("assets/maps/desert/meta-map-sm.png")
        const img = await jimp.read(process.argv[2])
        const stamps = await TiledMap.fromFile(process.argv[3])
        const worldWidth = WANGSIZE * img.bitmap.width
        const worldHeight = WANGSIZE * img.bitmap.height
        const finalMap = TiledMap.createEmpty(worldWidth,worldHeight, stamps.getConf())

        const colorGrids = getWangColorGrids(stamps)
        const colorLayerGrids = colorGrids.map(item =>
            applyPixelWangs(item[1], WANGSIZE, item[0], img));

        finalMap.applyLgs(colorLayerGrids, "color")

        const alphaMap = scanAlphaToGrid(img)
        const normalAlpha= normalizeGrid(alphaMap)
        const altMap = snapNormalGrid(normalAlpha,3)
        const cliffGridIndex = stamps.getLayers().find(l => l.name === CLIFFLAYERNAME).id
        // console.log(cliffGridIndex)
        // console.log(alphaMap.print())
        // console.log(normalAlpha.print())
        console.log(altMap.print())

        let cliffstampGrid = stamps.lg[cliffGridIndex]
        const cliffRegion = checkTiledLayerProperty(stamps.getConf(),cliffGridIndex,CLIFFLAYERNAME+"-region")
        if(cliffRegion){
            const pRegion = cliffRegion.split('-').map(i=>parseInt(i))
            cliffstampGrid = getSubArr(pRegion[0], pRegion[1],pRegion[2], pRegion[3], cliffstampGrid)
        } else {
            throw Error("Is cliff region specified?")
        }
        const replacers = getReplacementSet(stamps, CLIFFLAYERNAME)
        const replacers2 = getReplacementSet(stamps, CLIFFLAYERNAME, 2)
        const cliffLayerGrids = applyCliffs(cliffstampGrid, CLIFFLAYERNAME, altMap,[replacers,replacers2])
        // const cliffLayerGrids = applyCliffs(stamps.lg[cliffGridIndex], CLIFFLAYERNAME, altMap,[replacers,replacers2])
        finalMap.applyLgs(cliffLayerGrids, "cliff",true)


        console.log("got here")
        await finalMap.write('assets/maps/desert/ttmap.json')
    })()
