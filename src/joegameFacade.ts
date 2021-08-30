import gameconfig from './gameconfig'
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
import { parseCSVRowsToGameData } from 'utils/parseCSVRowsToGameData'

export default class joegameFacade extends IjoegameFacade {
    async initGame(baseURL: string): Promise<Phaser.Game> {
        const datastr = await (
            await fetch(baseURL + "assets/data.csv")
        ).text()
        const data = parseCSVRowsToGameData(datastr)
        const g = new Phaser.Game(gameconfig)
        g.scene.add('gameinit', class extends Phaser.Scene {
            preload() {
                // TODO properly ignore this in some typescript way
                this.load.setBaseURL(baseURL)
                this.registry.set('loaderBaseURL', baseURL)
                this.load.json('gdata', data)
                console.log(' gameinit preload')
                // rawmap
                // this.load.json(getMapKeyNameRaw(data.mapjson),data.mapjson)
            }
        }, true, {})
        return new Promise<Phaser.Game>((res,rej)=>{
            g.events.on('ready',()=>{
                console.log('game is ready!')
                res(g)
            })
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
