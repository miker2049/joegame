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
                gameLoaded:{}
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
    },
    guards:{},
    activities:{},
    delays:{}
}
