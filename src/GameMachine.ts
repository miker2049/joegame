import 'phaser'
import { Machine, MachineConfig, MachineOptions, assign } from 'xstate';
import IjoegameFacade from './IjoegameFacade';
interface GameMachineContext {
    game: Phaser.Game
    currentMap: 'string'
    facade: IjoegameFacade
}
interface GameMachineStateSchema {
    states: {
        initializing: {
            states: {
                creatingGame:{}
                loadingMapJSON:{}
                loadingAssets:{}
            }
        }
        inMenu: {}
        gameplay:{}
        inWiki: {}
        inDialogue: {}
    }
}

const GameMachineOptions: MachineOptions<GameMachineContext, any> = {
    actions:{
    },
    services:{
        createGame:(context, _event)=>context.facade.initGame(),
        loadMapJson:(context, _event)=>context.facade.loadMapJSON(context.game,context.currentMap),
        loadMapAssets:(context, _event)=>context.facade.loadAssets(context.game,context.currentMap)

    },
    guards:{},
    activities:{},
    delays:{}
}
