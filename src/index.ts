import MIDIPlayer from 'timidity-wasm'
import runCinematicNode from './actions/runCinematicNode'
import createTweetConvo from './factories/createTweetConvo'
import joegameFacade from './joegameFacade'
import shaders from './shaders/index'
import { parseCSVRowsToWikiData } from "./utils/parseCSVRowsToWikiData"
import { parsewikidata as parseOrgWikiData } from "./utils/parseWikiData"
import { loadMap } from './loadMap'
import { happyEmojiReact, sparkleCircle } from 'components/CharEmojiReaction'

async function playMIDIFile(path: string, context?: AudioContext) {

    const mplayer = await MIDIPlayer.createMIDIPlayer(BASEURL+"gravis.cfg",context)
    await mplayer.load(BASEURL+path)
    return mplayer
}

export {
    joegameFacade,
    runCinematicNode,
    createTweetConvo,
    parseOrgWikiData, //
    parseCSVRowsToWikiData,
    shaders,
    playMIDIFile,
    happyEmojiReact,
    sparkleCircle,
    loadMap
}
