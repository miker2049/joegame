import MIDIPlayer from 'timidity-wasm'
import runCinematicNode from './actions/runCinematicNode'
import createTweetConvo from './factories/createTweetConvo'
import joegameFacade from './joegameFacade'
import shaders from './shaders/index'
import { parseCSVRowsToGameData } from "./utils/parseCSVRowsToGameData"
import { parsewikidata as parseOrgWikiData } from "./utils/parseWikiData"
import { loadMap } from './loadMap'
import { happyEmojiReact, sparkleCircle } from 'components/CharEmojiReaction'
import { Menu } from 'components/ui/Menu'

// @ts-ignore
const BASEURL_GLOBAL: string = BASEURL

async function playMIDIFile(path: string, context?: AudioContext) {
    const mplayer = await MIDIPlayer.createMIDIPlayer(BASEURL_GLOBAL,context)
    await mplayer.load(BASEURL_GLOBAL+path)
    return mplayer
}

export {
    joegameFacade,
    runCinematicNode,
    createTweetConvo,
    parseOrgWikiData, //
    parseCSVRowsToGameData,
    shaders,
    playMIDIFile,
    happyEmojiReact,
    sparkleCircle,
    Menu,
    loadMap
}
