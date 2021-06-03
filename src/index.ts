import MIDIPlayer from 'timidity-wasm'
import runCinematicNode from './actions/runCinematicNode'
import createTweetConvo from './factories/createTweetConvo'
import joegameFacade from './joegameFacade'
import shaders from './shaders/index'
import { parseCSVRowsToWikiData } from "./utils/parseCSVRowsToWikiData"
import { parsewikidata as parseOrgWikiData } from "./utils/parseWikiData"
import { loadMap } from './loadMap'

async function playMIDIFile(path: string) {
    const mplayer = await MIDIPlayer.createMIDIPlayer("./gravis.cfg")
    mplayer.load(path)
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
    loadMap
}
