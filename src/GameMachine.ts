import joegameFacade from 'joegameFacade';
import 'phaser'
import { parseCSVRowsToGameData } from 'utils/parseCSVRowsToGameData';
import { Machine, MachineConfig, MachineOptions, assign } from 'xstate';
import IjoegameFacade from './IjoegameFacade';
interface GameMachineContext {
    game: Phaser.Game | undefined
    currentLevel: 'string' | undefined
    facade: IjoegameFacade
    baseURL: string

}

type GameMachineEvent =
    | { type: 'LOAD_LEVEL', level: string }

function createGameMachineConfig(baseURL: string): MachineConfig<GameMachineContext, any, GameMachineEvent> {
    return {
        initial: 'nothing',
        context: {
            facade: new joegameFacade(),
            game: undefined,
            currentLevel: undefined,
            baseURL
        },
        states: {
            nothing: {
                invoke: {
                    src: 'loadGame',
                    onDone: {
                        target: 'inBeginMenu',
                        actions: assign({
                            game: (_context, event) => event.data,
                        })
                    },
                    onError: {
                        actions: () => console.error("Cant load game with data")
                    }
                }
            },
            inBeginMenu: {},
            inMenu: {},
            gameplay: {},
            inCinematic: {},
            inWiki: {},
            inDialogue: {},
        }
    }
}


const GameMachineOptions: MachineOptions<GameMachineContext, GameMachineEvent> = {
    actions: {},
    services: {
        /*
         * This async function is a "service" to xstate, and is automatically invoked
         * by the game machine at its nothing state.
         *
         * We go from nothing, to something.
         * In truth, we already have a bunch of things in nothing land:
         * - a `joegameFacade`, given to us by the initial gameMachine context
         * - a `baseURL` which is how joegame knows where it is in a given dir structure
         *
         * What does adding a game to a context give us?
         * Not just a `Phaser.Game`, but a game with some default joegame data and defaults,
         * most prominently a ready parsed `wikiData` (which is named so vestigially)
         *
         * The wikidata is the source of truth for all assets as well as
         * entities (Character-Object)
         *
         * ## Go from nothing to something
         */
        loadGame: async (context) => {
            const datastr = await (
                await fetch(context.baseURL + "assets/data.csv")
            ).text()
            const data = parseCSVRowsToGameData(datastr)
            return await context.facade.initGame(data, context.baseURL)
        }
    },
    guards: {},
    activities: {},
    delays: {}
}
