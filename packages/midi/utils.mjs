export async function createSynth(acontext, workletURL) {
    try {
        await acontext.audioWorklet.addModule(workletURL)
    } catch (err) {
        console.log(err)
    }
    const node = new AudioWorkletNode(acontext, 'synth', {
        outputChannelCount: [2],
        processorOptions: {
        }
    })

    return await new Promise((res, rej) => {
        node.port.onmessage = (msg) => {
            console.log(msg)
            if(msg.data === 'INITIALIZED')
                res(node)
        }
        setTimeout(()=>rej('Problem initializing'), 4000)

    })
}

export async function loadFont(node, sfURL) {
    const sffile = await (await fetch(sfURL)).arrayBuffer()
    const arr = new Uint8Array(sffile)
    return await new Promise((res, rej) => {
        node.port.postMessage({ type: 'loadsf', sfdata: arr, isogg: 0 })
        node.port.onmessage = (msg) => {
            console.log(msg)
            if(msg.data === 'SFLOADED')
                res(node)
        }
        setTimeout(()=>rej('Problem loading SF'), 4000)

    })
}
export function playMidi(node){
    node.port.postMessage({type: 'play'})
}
export function pauseMidi(node){
    node.port.postMessage({type: 'pause'})
}
export function stopMidi(node){
    node.port.postMessage({type: 'stop'})
}
export async function loadMidi(node, midiURL) {
    const midifile = await (await fetch(midiURL)).arrayBuffer()
    const arr = new Uint8Array(midifile)
    return await new Promise((res, rej) => {
        node.port.postMessage({ type: 'loadmidi', mididata: arr, isogg: 0 })
        node.port.onmessage = (msg) => {
            console.log(msg)
            if(msg.data === 'MIDILOADED')
                res(node)
        }
        setTimeout(()=>rej('Problem loading midi.'), 4000)

    })
}

export async function createWithFont(acontext, workletURL, sfURL){
    let node
    try {
        node = await createSynth(acontext,workletURL)
        node = await loadFont(node, sfURL)
    } catch (err){
        throw new Error(err)
    }
    return node
}
export async function createWithFontAndMIDI(acontext, workletURL, sfURL, midiURL){
    let node
    try {
        node = await createSynth(acontext,workletURL)
        node = await loadFont(node, sfURL)
        node = await loadMidi(node, midiURL)
    } catch (err){
        throw new Error(err)
    }
    return node
}
