import { happyEmojiReact } from 'components/CharEmojiReaction'
import IjoegameFacade from 'IjoegameFacade'
import { ILevelComponents } from "ILevel"
import { parseCSVRowsToWikiData } from 'index'
import joegameFacade from "joegameFacade"
import defaults from './defaults'
import { ILevelConfig } from './ILevelConfig'

export async function loadMap(mapjsonfile: string,
    baseURL: string,
    datapath: string,
    plyr: { x: number, y: number },
    lvlCfg?: ILevelConfig): Promise<[ILevelComponents, IjoegameFacade]> {

    const fac = new joegameFacade()
    const config = Object.assign(defaults.levelConfig, lvlCfg)
    const datastr = await (await fetch(BASEURL+datapath)).text()
    const data = parseCSVRowsToWikiData(datastr)
    const game: Phaser.Game = await fac.initGame(data, BASEURL)
    await fac.loadMapJSON(game, mapjsonfile)
    await fac.loadAssets(game, mapjsonfile)
    await fac.loadConvoManifestJSON(game)
    fac.createAnims(game)
    fac.createDepthMap(game, mapjsonfile)
    const level = fac.runLevelScene(game, mapjsonfile)

    // switch to not turn on player
    if ((plyr.x > 0) && (plyr.y > 0)) {
        const player = fac.addPlayerToLevel(level, plyr.x, plyr.y, "player")
        level.scene.cameras.main.startFollow(player, false, 0.5, 0.5)
    }
    if (config.objectLayers) {
        config.objectLayers.forEach(layer => fac.addAllObjectsFromLayer(level, layer));
    }
    if (config.platformLayers) {
        config.platformLayers.forEach(layer => fac.addAllPlatformsFromLayer(level, layer));
    }
    if (config.npcLayers) {
        config.npcLayers.forEach(layer => fac.addAllPlatformsFromLayer(level, layer));
    }
    fac.createLevelPhysics(level)
    level.machineRegistry.startAll()

    level.scene.cameras.main.setZoom(config.zoom)
    level.scene.cameras.main.setBounds(0,0,level.map.widthInPixels,level.map.heightInPixels)

    const convos = await fac.addAllTweetConvosFromLayer(level, config.convosLayers)

    if (convos) {
        Promise.all(convos.map(con => con.runConvo()));
    }

    return [level, fac]
}
