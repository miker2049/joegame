export default class MIDIClient {

    presets: string[]
    midifileName: string | undefined
    soundfontName: string | undefined
    node: AudioWorkletNode

    constructor(node: AudioWorkletNode) {
        this.node = node
        this.presets = []
    }

    noteon(note: number, preset: number, vel: number) {
        this.node.port.postMessage({ type: 'on', note, preset, vel })
    }

    noteoff(note: number, preset: number, vel: number) {
        this.node.port.postMessage({ type: 'off', note, preset, vel })
    }


    playMidi() {
        this.node.port.postMessage({ type: 'play' })
    }
    pauseMidi() {
        this.node.port.postMessage({ type: 'pause' })
    }
    stopMidi() {
        this.node.port.postMessage({ type: 'stop' })
    }

    async loadMidi(midiURL: string) {
        const midifile = await (await fetch(midiURL)).arrayBuffer()
        const arr = new Uint8Array(midifile)
        return await new Promise((res, rej) => {
            this.node.port.postMessage({ type: 'loadmidi', mididata: arr, isogg: 0 })
            this.node.port.onmessage = (msg) => {
                if (msg.data === 'MIDILOADED') {
                    this.midifileName = midiURL
                    this.node.port.onmessage = NOOP
                    res(this.node)
                }
            }
            setTimeout(() => rej('Problem loading midi.'), 4000)

        })
    }

    async loadFontFile(sfURL: string): Promise<MIDIClient> {
        const sffile = await (await fetch(sfURL)).arrayBuffer()
        const arr = new Uint8Array(sffile)
        return await new Promise((res, rej) => {
            this.node.port.postMessage({ type: 'loadsf', sfdata: arr, isogg: 0 })
            this.node.port.onmessage = (msg) => {
                if (msg.data === 'SFLOADED') {
                    this.soundfontName = sfURL
                    this.node.port.onmessage = NOOP
                    res(this)
                }
            }
            setTimeout(() => rej('Problem loading SF'), 4000)
        })
    }

    async loadFont(sfURL: string): Promise<MIDIClient> {
        await this.loadFontFile(sfURL)
        await this.getPresets()
        return this
    }

    async getPresets() {
        this.node.port.postMessage({ type: 'getpresetnames' })
        const names = await new Promise<string[]>((res, rej) => {
            this.node.port.onmessage = (msg) => {
                if (msg.data.type === 'presetnames')
                    res(msg.data.names)
            }
            setTimeout(() => rej('Problem initializing'), 4000)

        })
        this.presets = names
        console.log(this.presets)
    }

    static async create(acontext: AudioContext, workletURL: string): Promise<MIDIClient> {
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
                if (msg.data === 'INITIALIZED')
                    res(new MIDIClient(node))
            }
            setTimeout(() => rej('Problem initializing'), 4000)

        })
    }

    static async createWithFontAndMIDI(acontext: AudioContext, workletURL: string, sfURL: string, mURL: string) {
        const cl = await MIDIClient.create(acontext, workletURL)
        await cl.loadFont(sfURL)
        await cl.loadMidi(mURL)
        return cl
    }
}

function NOOP(_arg: any): void {
    return
}
