import { createMenu, Menu } from './components/ui/Menu'
import { ILevelComponents } from './ILevel'
import { joegameFacade as fac } from './joegameFacade'
import 'phaser'
import {
  Machine,
  MachineConfig,
  MachineOptions,
  assign,
  interpret,
  createMachine,
  send
} from 'xstate'

interface GameMachineContext {
  game: Phaser.Game | undefined
  currentMenu: undefined | Menu
  currentLevel: undefined | ILevelComponents
  baseURL: string
}

type GameMachineEvent =
  | { type: 'LOAD_LEVEL'; level: string }
  | { type: 'ARROWDOWN' }

type GameMachineTypeStates =
  | {
      value: 'nothing'
      context: GameMachineContext
    }
  | {
      value: 'inBeginMenu'
      context: GameMachineContext & {
        game: Phaser.Game
        currentFocus: Menu
      }
    }

function createGameMachineConfig(
  baseURL: string
): MachineConfig<GameMachineContext, any, GameMachineEvent> {
  return {
    initial: 'nothing',
    id: 'game-machine',
    context: {
      game: undefined,
      currentMenu: undefined,
      currentLevel: undefined,
      baseURL
    },
    states: {
      nothing: {
        invoke: {
          src: (context) => fac.initGame(context.baseURL, {}),
          onDone: {
            target: 'inBeginMenu',
            actions: assign({
              game: (_context, event) => event.data
            })
          },
          onError: {
            actions: () => console.error('Cant load game with data')
          }
        }
      },
      inBeginMenu: {
        entry: { type: 'openMenu' },
        on: {}
      },
      inLevel: {
        states: {
          inMenu: {},
          gameplay: {},
          inCinematic: {},
          inWiki: {},
          inDialogue: {}
        }
      }
    }
  }
}

const GameMachineOptions: MachineOptions<GameMachineContext, GameMachineEvent> =
  {
    actions: {
      openMenu: (context) => {
        if (context.currentMenu && context.game) {
          context.currentMenu.open()
        }
      }
    },
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
      // loadGame: (context) => { return async (context) => context.facade.initGame( context.baseURL) }
    },
    guards: {},
    activities: {},
    delays: {}
  }

export function startGameService(ur: string) {
  const mach = createMachine(createGameMachineConfig(ur), GameMachineOptions)
  const service = interpret(mach, { devTools: false })
  service.start()
  return service
}
