import createGameConfig from './gameconfig'
import IjoegameFacade from './IjoegameFacade'
import loadMapJSON from './utils/loadMapJSON'
import loadMapAssets from './utils/loadMapAssets'
import createAnims from './utils/createAnims'
import { getSceneKeyName } from './utils/getKeyNames'
import createBaseLevel from './factories/createBaseLevel'
import { IWikiData } from './utils/parseWikiData'
import addAllNPCsFromLayer from './actions/addAllNPCsFromLayer'
import addAllTweetConvosFromLayer from './actions/addAllTweetConvosFromLayer'
import addAllObjectsFromLayer from './actions/addAllObjectsFromLayer'
import addAllPlatformsFromLayer from './actions/addAllPlatformsFromLayer'
import addPlayerToLevel from './actions/addPlayerToLevel'
import createLevelPhysics from './factories/createLevelPhysics'
import createDepthMap from './utils/createDepthMap'
import runCinematicNode from './actions/runCinematicNode'
import createTweetConvo from './factories/createTweetConvo'
import loadConvoManifestJSON from './utils/loadConvoManifestJSON'
import { parseCSVRowsToWikiData } from 'index'
import defaults from './defaults'
import { ILevelConfig } from './ILevelConfig'
import { ILevelComponents } from 'ILevel'

export default class joegameFacade extends IjoegameFacade {
    initGame(gdata: IWikiData, baseURL: string): Promise<Phaser.Game> {
        return new Promise((resolve, reject) => {
            new Phaser.Game(createGameConfig(gdata, baseURL, resolve))
        })
    }
    loadMapJSON(game: Phaser.Game, mapjsonpath: string): Promise<Phaser.Game> {
        return loadMapJSON(game, mapjsonpath)
    }
    loadAssets(game: Phaser.Game, mapjsonpath: string): Promise<Phaser.Game> {
        return loadMapAssets(game, mapjsonpath)
    }
    loadConvoManifestJSON(game: Phaser.Game): Promise<Phaser.Game> {
        return loadConvoManifestJSON(game)
    }
    createAnims = createAnims

    runLevelScene = createBaseLevel

    addAllNPCsFromLayer = addAllNPCsFromLayer
    addAllTweetConvosFromLayer = addAllTweetConvosFromLayer
    addAllObjectsFromLayer = addAllObjectsFromLayer
    addAllPlatformsFromLayer = addAllPlatformsFromLayer
    addPlayerToLevel = addPlayerToLevel
    createLevelPhysics = createLevelPhysics
    createDepthMap = createDepthMap
    runCinematicNode = runCinematicNode
    createTweetConvo = createTweetConvo

    async loadMap(mapjsonfile: string,
        baseURL: string,
        data: IWikiData,
        plyr: { x: number, y: number },
        lvlCfg?: ILevelConfig): Promise<ILevelComponents> {
        const config = lvlCfg ?? defaults.levelConfig
        const game: Phaser.Game = await this.initGame(data, baseURL)
        await this.loadMapJSON(game, mapjsonfile)
        await this.loadAssets(game, mapjsonfile)
        this.createAnims(game)
        this.createDepthMap(game, mapjsonfile)
        const level = this.runLevelScene(game, mapjsonfile)

        // switch to not turn on player
        if ((plyr.x > 0) && (plyr.y > 0)) {
            const player = this.addPlayerToLevel(level, plyr.x, plyr.y)
            level.scene.cameras.main.startFollow(player, false, 0.5, 0.5)
        }
        if (config.objectLayers) {
            config.objectLayers.forEach(layer => this.addAllObjectsFromLayer(level, layer));
        }
        if (config.platformLayers) {
            config.platformLayers.forEach(layer => this.addAllPlatformsFromLayer(level, layer));
        }
        if (config.npcLayers) {
            config.npcLayers.forEach(layer => this.addAllPlatformsFromLayer(level, layer));
        }
        this.createLevelPhysics(level)
        level.scene.cameras.main.setZoom(config.zoom)
        return level
    }

}
