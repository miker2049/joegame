import { parseCSVRowsToWikiData } from 'index'
import defaults from './defaults'
import { ILevelConfig } from './ILevelConfig'
import { ILevelComponents } from "ILevel"
import joegameFacade from "joegameFacade"

export async function loadMap(mapjsonfile: string,
    baseURL: string,
    datapath: string,
    plyr: { x: number, y: number },
    lvlCfg?: ILevelConfig): Promise<ILevelComponents> {
    const fac = new joegameFacade()
    const config = lvlCfg ?? defaults.levelConfig
    const datastr = await (await fetch(datapath)).text()
    const data = parseCSVRowsToWikiData(datastr)
    const game: Phaser.Game = await fac.initGame(data, baseURL)
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

    const convos = await fac.addAllTweetConvosFromLayer(level, 'TweetConvos')
    if (convos) {
        Promise.all(convos.map(con => con.runConvo()));
    }
    return level
}
