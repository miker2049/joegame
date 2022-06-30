export async function createSynth(acontext, workletURL) {
    try {
        await acontext.audioWorklet.addModule(workletURL)
    } catch (err) {
        console.log(err)
    }
    return new AudioWorkletNode(acontext, 'synth', {
        outputChannelCount: [2],
        processorOptions: {}
    })
}

export async function loadFont(node, sfURL) {
    const sffile = await (await fetch(sfURL)).arrayBuffer()
    const arr = new Uint8Array(sffile)
    node.port.postMessage({ type: "loadsf", sfdata: arr, isogg: 0 })
}
