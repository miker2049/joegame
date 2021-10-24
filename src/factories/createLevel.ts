import { ILevelConfig } from "ILevelConfig"
import { joegameFacade as fac } from "joegameFacade"
import loadAfterLoad from "../utils/loadAfterLoad"
import d from '../defaults'

export async function createLevel(game: Phaser.Game, config: ILevelConfig) {
    config = Object.assign(d.levelConfig, config)

    await fac.loadMapJSON(game, config.mapPath)
    await fac.loadAssets(game, config)
    await fac.loadConvoManifestJSON(game)
    fac.createAnims(game, config.mapPath)
    fac.createDepthMap(game, config.mapPath)
    const level = fac.runLevelScene(game, config)

    // switch to not turn on player
    level.scene.cameras.main.startFollow(level.player, false, 0.5, 0.5)

    if (config.objectLayers) {
        config.objectLayers.forEach(layer => fac.addAllObjectsFromLayer(level, layer));
    }
    if (config.platformLayers) {
        config.platformLayers.forEach(layer => fac.addAllPlatformsFromLayer(level, layer));
    }
    if (config.npcLayers) {
        config.npcLayers.forEach(layer => fac.addAllNPCsFromLayer(level, layer));
    }
    if (config.lightLayers) {
        console.log(config)
        config.lightLayers.forEach(layer => fac.addAllLightsFromLayer(level, layer));
    }
    fac.createLevelPhysics(level)
    level.machineRegistry.startAll()

    level.scene.cameras.main.setZoom(config.zoom)
    level.scene.cameras.main.setBounds(0, 0, level.map.widthInPixels, level.map.heightInPixels)


    if (config.dialogueScript && config.dialogueScriptFormat) {
        loadAfterLoad(level.scene, 'cine-dial', config.dialogueScript, config.dialogueScriptFormat).then(key => {
            const yarnjson = level.scene.cache[config.dialogueScriptFormat].get(key)
            if (config.runDialogue) {
                fac.runCinematicNode(level, 'Start', yarnjson)
            }
        })
    }

    if (config.runConvos) {
        //no need to await,
        Promise.all(config.convosLayers.map(l => fac.addAllTweetConvosFromLayer(level, l)))
    }

    if (config.playMusic) {
        fac.loadMIDIFile(config.musicPath).then(plyr => {
            level.scene.sound.once(Phaser.Sound.Events.UNLOCKED, () => {
                // level.scene.music.play()
                plyr.play()
            })
        })
    }

    if (config.lights){
        level.scene.lights.enable()
    }
    return level
}
