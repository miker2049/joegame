export async function createSynth(acontext, workletURL) {
    try {
        await acontext.audioWorklet.addModule(workletURL)
    } catch (err) {
        console.log(err)
    }
    const node = new AudioWorkletNode(acontext, 'synth', {
        outputChannelCount: [2],
        processorOptions: {}
    })

    return await new Promise((res, rej) => {
        node.port.onmessage = (msg) => {
            console.log(msg)
            if(msg.data === 'INITIALIZED')
                res(node)
        }
        // setTimeout(()=>res(node), 1000)

    })
}

export async function loadFont(node, sfURL) {
    const sffile = await (await fetch(sfURL)).arrayBuffer()
    const arr = new Uint8Array(sffile)
    node.port.postMessage({ type: "loadsf", sfdata: arr, isogg: 0 })
}
