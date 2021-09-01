import defaults from "defaults"
import {joegameFacade as fac } from "joegameFacade"
import { ILevelConfig } from "ILevelConfig"

export async function createLevel(mapjsonfile: string,
    game: Phaser.Game,
    plyr: { x: number, y: number },
    lvlCfg?: ILevelConfig) {

    const config = Object.assign(defaults.levelConfig, lvlCfg)
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
    level.scene.cameras.main.setBounds(0, 0, level.map.widthInPixels, level.map.heightInPixels)

    await fac.addAllTweetConvosFromLayer(level, config.convosLayers)
    return level
}
