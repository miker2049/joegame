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

export default class joegameFacade extends IjoegameFacade {
    initGame(gdata: IWikiData): Promise<Phaser.Game> {
        console.log('this here hoheee?')
        return new Promise((resolve, reject) => {
            new Phaser.Game(createGameConfig(gdata, resolve))
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

}
