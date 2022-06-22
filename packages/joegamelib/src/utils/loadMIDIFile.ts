import MIDIPlayer from "timidity-wasm"

// @ts-ignore
const BASEURL_GLOBAL: string = BASEURL

export default async function(path: string, context?: AudioContext) {
    const mplayer = await MIDIPlayer.createMIDIPlayer(BASEURL_GLOBAL,context)
    await mplayer.load(BASEURL_GLOBAL+path)
    return mplayer
}
