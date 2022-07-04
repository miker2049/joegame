import Synth from '../../libsfsynth'
// https://stackoverflow.com/a/49448982
type pointer = number

type ProcessorInMessages = {
    sfdata: Uint8Array
    type: "loadsf"
} | {
    sfdata: Uint8Array
    type: "loadmidi"
} | {
    type: "play"
} | {
    type: "pause"
} | {
    type: "stop"
}

registerProcessor('synth', class extends AudioWorkletProcessor {
    _qu: any[]
    _midifile: pointer | undefined
    _midifile_start: pointer | undefined
    _playing: boolean
    _secs: number
    _msstep: number = 128 * (1000 / 44100)
    _synth: pointer

    ready: boolean
    _arr: Float32Array
    _lib: any
    constructor(_args: any) {
        super()
        this.ready = false
        Synth({
            print: (a) => this.port.postMessage(a),
            printErr: (a) => this.port.postMessage(a)
        }).then(mod => {
            this._lib = mod
            this.ready = true
            this.port.postMessage(`INITIALIZED`)
        }).catch(err => console.log(err))
        this._arr = new Float32Array(128 * 4 * 2)
        this.port.onmessage = this._handleMessage.bind(this)
        this.port.onmessageerror = this._handleMessage.bind(this)
        this._qu = []
        this._midifile = undefined
        this._midifile_start = undefined
        this._playing = false
        this._secs = 0
    }

    _handleMessage(message) {
        if (!this.ready)
            this.port.postMessage(`not ready`)
        switch (message.data.type) {
            case "loadsf": {
                this.port.postMessage(`before sfload`)
                const sfptr = this._lib._malloc(message.data.sfdata.byteLength);
                this._lib.HEAPU8.set(message.data.sfdata, sfptr)
                this._synth = this._lib._init_web(sfptr,
                    message.data.sfdata.byteLength,
                    message.data.isogg)
                this.port.postMessage(`SFLOADED`)
                this._qu = []
                // const presetCount = this._lib._get_presets_count(this._synth)
                // for (let i = 0; i < presetCount; i++) {
                //     const preset = this._lib._get_preset(this._synth, i)
                //     let name = undefined
                //     if (preset)
                //         name = this._lib.UTF8ToString(preset)
                //     if (name)
                //         this.port.postMessage(`SFLOADED ${name}`)
                // }
                break;
            }
            case "loadmidi": {
                const size = message.data.mididata.byteLength
                this.port.postMessage(`before midiload`)
                const midiptr = this._lib._malloc(size);
                this._lib.HEAPU8.set(message.data.mididata, midiptr)
                this._midifile = this._lib._load_midi_web(midiptr,
                    size)
                this._midifile_start = this._midifile
                this.port.postMessage(`MIDILOADED`)

                this.port.postMessage(`this._midifile ${this._midifile}`)
                this.port.postMessage(`this._midifile ptr ${midiptr}`)
                this.port.postMessage(`this._midifile ptr ${message.data.mididata.byteLength}`)

                break;
            }
            case "on": {
                let note = message.data.note ?? 60
                this._qu.push([0, 7, note, 1])
                break;
            }
            case "play": {
                this._playing = true
                break;
            }
            case "pause": {
                this._playing = false
                break;
            }
            case "stop": {
                this._playing = false
                this._secs = 0
                this._midifile = this._midifile_start
                break;
            }
            case "off": {
                let note = message.data.note ?? 60
                this._qu.push([1, 7, note, 1])
                break;
            }
            case "here": {
                this.port.postMessage("here")
                break;
            }
            default: this.port.postMessage(`default trig`)

        }
    }

    process(_input, output) {
        const outputs = output[0]
        if (this._playing) {
            this._midifile = this._lib._process_midi_web(this._midifile, this._secs, this._synth)
            this._secs += this._msstep
        }
        this._qu.forEach(ev => {
            if (ev[0] === 0) {
                this._lib._noteon_web(this._synth, ev[1], ev[2], ev[3])
            } else if (ev[0] === 1) {
                this._lib._noteoff_web(this._synth, ev[1], ev[2], ev[3])
            }
        });
        const buff = this._lib._process_web(this._synth)
        this._qu = []

        outputs[0].set(this._lib.HEAPF32.subarray(buff / 4, buff / 4 + 128))
        outputs[1].set(this._lib.HEAPF32.subarray(buff / 4 + 128, buff / 4 + 128 * 2))
        return true
    }
})
