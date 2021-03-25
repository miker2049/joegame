import createGameConfig from './gameconfig'
import IjoegameFacade from './IjoegameFacade'
import loadMapJSON from './utils/loadMapJSON'
import loadMapAssets from './utils/loadMapAssets'
import createAnims from './utils/createAnims'
import {getSceneKeyName} from './utils/getKeyNames'
import createBaseLevel from './factories/createBaseLevel'

export default class joegameFacade extends IjoegameFacade {
    initGame(): Promise<Phaser.Game>{
        return new Promise((resolve, reject)=>{
            new Phaser.Game(createGameConfig(resolve))
        })
    }
    loadMapJSON(game: Phaser.Game, mapjsonpath:string): Promise<Phaser.Game>{
        return loadMapJSON(game,mapjsonpath)
    }
    loadAssets(game: Phaser.Game, mapjsonpath:string): Promise<Phaser.Game>{
        return loadMapAssets(game,mapjsonpath)
    }
    createAnims = createAnims

    runLevelScene = createBaseLevel

}
