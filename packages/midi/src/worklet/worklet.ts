import Synth from '../../libsfsynth'

// https://stackoverflow.com/a/49448982
type pointer = number
type LoadSynthFunction = (conf: any) => Promise<SfSynthModule>

interface SfSynthModule extends EmscriptenModule {
    _init_web(ptr: pointer, s: number): pointer
    _process_web(s: pointer): pointer
    _noteoff_web(s: pointer, preset: number, note: number, vel: number): void
    _noteon_web(s: pointer, preset: number, note: number, vel: number): void
    _process_midi_web(m: pointer, sec: number, s: pointer): pointer
    _load_midi_web(m: pointer, s: number): pointer
    _tsf_note_off_all(s: pointer): void
    _get_presets_count(s: pointer): number
    _get_preset(s: pointer, i: number): pointer
    _seek_to_msec_web(m: pointer, msec: number): pointer
    _get_midi_total_msec_web(s: pointer): number
    UTF8ToString(p: pointer): string
}

export type ProcessorInMessages = {
    sfdata: Uint8Array
    type: "loadsf"
} | {
    mididata: Uint8Array
    type: "loadmidi"
} | {
    type: "play"
} | {
    type: "pause"
} | {
    type: "stop"
} | {
    type: "on",
    note: number
} | {
    type: "off",
    note: number
} | {
    type: "getpresetnames"
} | {
    type: "seekmidi",
    msec: number
}


registerProcessor('synth', class extends AudioWorkletProcessor {
    qu: any[]
    midifile: pointer | undefined
    midifile_start: pointer | undefined
    midifile_length: number | undefined
    playing: boolean
    secs: number
    msstep: number = 128 * (1000 / 44100)
    synth: pointer | undefined
    ready: boolean
    arr: Float32Array
    lib: SfSynthModule | undefined
    constructor(_args: any) {
        super()
        this.ready = false
        const loadSynth: LoadSynthFunction = Synth
        loadSynth({
            print: (a: any) => this.port.postMessage(a),
            printErr: (a: any) => this.port.postMessage(a)
        }).then((mod: SfSynthModule) => {
            this.lib = mod
            this.ready = true
            this.port.postMessage(`INITIALIZED`)
        }).catch(err => console.log(err))
        this.arr = new Float32Array(128 * 4 * 2)
        this.port.onmessage = this._handleMessage.bind(this)
        this.port.onmessageerror = this._handleMessage.bind(this)
        this.qu = []
        this.playing = false
        this.secs = 0
    }
    setOnHeap(data: Uint8Array): [ptr: pointer, size: number] {
        let size: number
        let ptr: number
        size = data.byteLength
        ptr = this.lib!._malloc(size);
        this.lib!.HEAPU8.set(data, ptr)
        return [ptr, size]
    }
    _loadsf(message: ProcessorInMessages & { type: 'loadsf' }) {
        this.port.postMessage(`before sfload`)
        const [sfptr, sfsize] = this.setOnHeap(message.sfdata)
        this.synth = this.lib!._init_web(sfptr, sfsize)
        this.port.postMessage(`SFLOADED ${this.synth}`)
        this.port.postMessage(`SFLOADED`)
    }
    _loadmidi(message: ProcessorInMessages & { type: 'loadmidi' }) {
        this.port.postMessage(`before sfload`)
        const [midiptr, midisize] = this.setOnHeap(message.mididata)
        this.midifile = this.lib!._load_midi_web(midiptr,
            midisize)
        this.midifile_start = this.midifile
        this.port.postMessage(`MIDILOADED`)
    }
    _seekmidi(message: ProcessorInMessages & { type: 'seekmidi' }) {
        if(!this.midifile_start || !this.midifile_length){
            this.port.postMessage(`Problem seeking`)
            return
        }
        if(message.msec > this.midifile_length || message.msec < 0){
            this.port.postMessage(`Problem seeking, invalid msec`)
            return
        }
        const out=this.lib!._seek_to_msec_web(this.midifile_start, message.msec)
        if(out){
            let inplace = false
            if(this.playing){
                this.playing = false
                inplace = true
            }
            this.secs = message.msec
            this.midifile = out
            this.port.postMessage(`seeked ${out}`)
            if(inplace){
                this.playing = true
            }
        }
    }

    _handleMessage(message: MessageEvent<ProcessorInMessages>) {
        if (!this.ready || !this.lib) {
            this.port.postMessage(`not ready`)
            return
        }
        switch (message.data.type) {
            case "getpresetnames": {
                if(!this.lib || !this.synth) break
                let names = []
                const presetCount = this.lib._get_presets_count(this.synth)
                for (let i = 0; i < presetCount; i++) {
                    const preset = this.lib._get_preset(this.synth, i)
                    let name = undefined
                    if (preset){
                        name = this.lib.UTF8ToString(preset)
                        names.push(name)
                    }
                }
                this.port.postMessage({type: 'presetnames', names: names} )
                break
            }
            case "loadsf": {
                this._loadsf(message.data)
                break;
            }
            case "loadmidi": {
                this._loadmidi(message.data)
                const dur = this.lib._get_midi_total_msec_web(this.midifile_start!)
                this.midifile_length = dur

                this.port.postMessage(`this.midifile ${this.midifile}`)
                this.port.postMessage(`this.synth ptr ${this.synth}`)
                this.port.postMessage(`midifile dur ${dur/1000}`)
                break;
            }
            case "on": {
                this.port.postMessage(`this.synth ptr ${this.synth}`)
                let note = message.data.note ?? 60
                this.qu.push([0, 7, note, 1])
                break;
            }
            case "seekmidi": {
                this.qu.push([9])
                this._seekmidi(message.data)
                break;
            }
            case "play": {
                this.playing = true

                this.port.postMessage(`seeked `)
                break;
            }
            case "pause": {
                this.playing = false
                this.qu.push([9])
                break;
            }
            case "stop": {
                this.qu.push([9])
                this.playing = false
                this.secs = 0
                this.midifile = this.midifile_start
                break;
            }
            case "off": {
                let note = message.data.note ?? 60
                this.qu.push([1, 7, note, 1])
                break;
            }
            default: this.port.postMessage(`default trig`)

        }
    }

    process(_input: Float32Array[][], output: Float32Array[][]) {
        if (!this.lib || !this.synth) {
            return true
        }
        const outputs = output[0]
        if (this.playing && this.midifile) {
            this.midifile = this.lib._process_midi_web(this.midifile,
                                                       this.secs,
                                                       this.synth)
            this.secs += this.msstep
        }
        this.qu.forEach(ev => {
            if (ev[0] === 0) {
                this.lib!._noteon_web(this.synth!, ev[1], ev[2], ev[3])
            } else if (ev[0] === 1) {
                this.lib!._noteoff_web(this.synth!, ev[1], ev[2], ev[3])
            } else if (ev[0]=== 9) {
                this.lib!._tsf_note_off_all(this.synth!)
            }
        });
        this.qu = []

        const buff = this.lib._process_web(this.synth)
        outputs[0].set(this.lib.HEAPF32.subarray(buff / 4, buff / 4 + 128))
        outputs[1].set(this.lib.HEAPF32.subarray(buff / 4 + 128, buff / 4 + 128 * 2))
        return true
    }
})
