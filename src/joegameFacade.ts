import addAllLightsFromLayer from 'actions/addAllLightsFromLayer'
import { createLevel } from 'factories/createLevel'
import { ILevelComponents } from 'ILevel'
import { ILevelConfig } from 'ILevelConfig'
import { Level } from 'Level'
import loadMIDIFile from 'utils/loadMIDIFile'
import { parseCSVRowsToGameData } from 'utils/parseCSVRowsToGameData'
import addAllNPCsFromLayer from './actions/addAllNPCsFromLayer'
import addAllObjectsFromLayer from './actions/addAllObjectsFromLayer'
import addAllPlatformsFromLayer from './actions/addAllPlatformsFromLayer'
import addAllTweetConvosFromLayer from './actions/addAllTweetConvosFromLayer'
import addPlayerToLevel from './actions/addPlayerToLevel'
import runCinematicNode from './actions/runCinematicNode'
import createLevelPhysics from './factories/createLevelPhysics'
import createTweetConvo from './factories/createTweetConvo'
import gameconfig from './gameconfig'
import IjoegameFacade from './IjoegameFacade'
import createAnims from './utils/createAnims'
import createDepthMap from './utils/createDepthMap'
import loadConvoManifestJSON from './utils/loadConvoManifestJSON'
import loadMapAssets from './utils/loadMapAssets'
import loadMapJSON from './utils/loadMapJSON'


/* ## Facade design principal
 * A "facade" with lots of cool stuff in it.
 *
 */
export const joegameFacade: IjoegameFacade = class {
    static async initGame(baseURL: string): Promise<Phaser.Game> {
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
    static loadMapJSON(game: Phaser.Game, mapjsonpath: string): Promise<Phaser.Game> {
        return loadMapJSON(game, mapjsonpath)
    }
    static loadAssets(game: Phaser.Game, config: ILevelConfig): Promise<Phaser.Game> {
        return loadMapAssets(game, config)
    }
    static loadConvoManifestJSON(game: Phaser.Game): Promise<Phaser.Game> {
        return loadConvoManifestJSON(game)
    }
    static createAnims = createAnims

    static runLevelScene = function(game: Phaser.Game, config: ILevelConfig): ILevelComponents {return new Level(game, config)}
    static createLevel = createLevel

    static loadMIDIFile = loadMIDIFile

    static addAllNPCsFromLayer = addAllNPCsFromLayer
    static addAllLightsFromLayer = addAllLightsFromLayer
    static addAllTweetConvosFromLayer = addAllTweetConvosFromLayer
    static addAllObjectsFromLayer = addAllObjectsFromLayer
    static addAllPlatformsFromLayer = addAllPlatformsFromLayer
    static addPlayerToLevel = addPlayerToLevel
    static createLevelPhysics = createLevelPhysics
    static createDepthMap = createDepthMap
    static runCinematicNode = runCinematicNode
    static createTweetConvo = createTweetConvo
}
