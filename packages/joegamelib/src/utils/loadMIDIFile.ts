import MIDIPlayer from "timidity"

// @ts-ignore
const BASEURL_GLOBAL: string = BASEURL

export default async function(path: string, context?: AudioContext) {
    const mplayer = await MIDIPlayer.createMIDIPlayer(BASEURL_GLOBAL,context)
    await mplayer.load(BASEURL_GLOBAL+path)
    return mplayer
}
