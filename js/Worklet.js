import Synth from '../libsynth'
import WASMAudioBuffer from './WASMAudioBuffer'
//
// https://stackoverflow.com/a/49448982

registerProcessor('synth', class extends AudioWorkletProcessor {
  // ...
  constructor(args) {
    super()
    this._lib = Synth()
    this._arr = new Float32Array(128*4*2)
    this.port.postMessage(`this buff is ${this.buff}`)
    this.port.onmessage = this._handleMessage.bind(this)
    this.port.onmessageerror = this._handleMessage.bind(this)
  }

  _handleMessage(message) {
    switch (message.data.type) {
      case "loadsf": {
        this.port.postMessage(`before sfload`)

        this._synth = this._lib._tsf_load_memory(message.data.sfdata, message.data.size)
        this._lib._tsf_set_max_voices(this._synth, 64)
        this._lib._tsf_set_output(this._synth,2,44100,-5)
        this._lib._tsf_set_volume(this._synth,1.0)
        this._buff = this._lib._malloc(128*4*2)
        this.port.postMessage(`after sfload, buff pointer is ${this._buff}`)
        this.ready = true
        break;
      }
      case "on": {
        const res = this._lib._tsf_channel_note_on(this._synth,2,60,1.0)
         this._lib._tsf_note_on(this._synth,2,Math.floor(Math.random*40)+20,1.0)
         this._lib._tsf_note_on(this._synth,2,Math.floor(Math.random*40)+20,1.0)
        this.port.postMessage(`after after voice count ${this._lib._tsf_active_voice_count(this._synth)}`)
        this.port.postMessage(`did a note on ${res}`)
        break;
      }
      case "off": {
        // this._lib._fluid_synth_noteoff(this.synth, 2, 60, 127)
        this.port.postMessage(`did a note off`)
        break;
      }
      case "here": {
        this.port.postMessage("here")
        break;
      }
      default:  this.port.postMessage(`default trig`)

    }
  }

  process(input, output) {
    if (!this.ready) return
    const outputs = output[0]
    this._lib._tsf_render_float(this._synth,this._buff,128,0)
    // const arr = new Float32Array(this._lib.HEAPU8, this._buff, 128*4*2)
    this._arr.set(this._lib.HEAPF32.subarray(this._buff/4, this._buff/4+128))
    for (var i = 0; i < 128; ++i) {
      outputs[0][i] = this._arr[i]
      outputs[1][i] = this._arr[i]
    }
    return true
  }
})
