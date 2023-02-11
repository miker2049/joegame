/**
 * @module joegame
 *
 */
// import MIDIPlayer from 'timidity'
// import runCinematicNode from './actions/runCinematicNode'
// import createTweetConvo from './factories/createTweetConvo'
// import { joegameFacade } from './joegameFacade'
// import shaders from './shaders/index'
// import { parseCSVRowsToGameData } from './utils/parseCSVRowsToGameData'
// import { parsewikidata as parseOrgWikiData } from './utils/parseWikiData'
// import { loadMap } from './loadMap'
// import { happyEmojiReact, sparkleCircle } from './components/CharEmojiReaction'
// import { createMenu } from './components/ui/Menu'
// import { startGameService } from './GameMachine'
// import loadAfterLoad from './utils/loadAfterLoad'
import TiledRawJSON from 'types/TiledRawJson'
import { embedTilesets } from './utils/loadMapJSON'
import { LevelScene } from './LevelScene'
import { objectsAndPack } from 'mapscripts/src/saturator'

// @ts-ignore
// const BASEURL_GLOBAL: string = BASEURL

// async function playMIDIFile(path: string, context?: AudioContext) {
//   const mplayer = await MIDIPlayer.createMIDIPlayer(BASEURL_GLOBAL, context)
//   await mplayer.load(BASEURL_GLOBAL + path)
//   return mplayer
// }

// const startGameMachineWithBaseURL = () => startGameService(BASEURL_GLOBAL)

/**
 @enum
 but there was other thing
 # hmmm
 more and more
 not sure where we went
 ## yes ok
 aksjd
 aksdj
 sdakja
 kasjdaksjda
 asd
 dasdasdasdadfs fdsafj sadkjfsa kdfjsa dksdjaf askjdf sdkjfbsarieuwabkfjasd nmcx,zvjbielusdjf,bkcxmnvewliusjfdb, cxzmn;ewikjdsz v,cxsjdf xzksdjfb
 jdf dslkjfdsa f
 dsaf nsdnf s;dkjfbsdf
 sdf 
*/

async function loadLevel(
  map: TiledRawJSON,
  key: string,
  gameConfigOverride?: Phaser.Types.Core.GameConfig
) {
  const embedded = await embedTilesets(map)
  const saturated = objectsAndPack(embedded)
  const _scene = new LevelScene(key, saturated)

  const defaultGameConfig = {
    type: Phaser.AUTO,
    pixelArt: true,
    parent: null as unknown as undefined,
    render: {
      transparent: true
    }
  }

  const finalConfig = {
    ...defaultGameConfig,
    ...gameConfigOverride
  }
  await new Promise<Phaser.Game>((res) => {
    const _game = new Phaser.Game(finalConfig)
    _game.scene.add(key, _scene, true)
    _game.events.once('ready', () => res(_game))
  })
  const scene = await new Promise<LevelScene>((res) => {
    _scene.events.once('levelready', () => res(_scene))
  })
  return scene
}

export {
  LevelScene,
  loadLevel
  // joegameFacade,
  // loadAfterLoad,
  // runCinematicNode,
  // startGameMachineWithBaseURL,
  // createTweetConvo,
  // parseOrgWikiData, //
  // parseCSVRowsToGameData,
  // shaders,
  // playMIDIFile,
  // happyEmojiReact,
  // sparkleCircle,
  // createMenu,
  // loadMap
}
