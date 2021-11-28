import Fluid from '../libfluidlite'
import WASMAudioBuffer from './WASMAudioBuffer'
//
// https://stackoverflow.com/a/49448982

registerProcessor('fluid', class extends AudioWorkletProcessor {
  // ...
  constructor(args) {
    super()
    this._lib = Fluid()
    this.synth = this._lib._fluid_web_init_synth()
    this.buff = this._lib._fluid_web_create_buffers();
    this.port.postMessage(`this buff is ${this.buff}`)
    this.port.onmessage = this._handleMessage.bind(this)
    this.port.onmessageerror = this._handleMessage.bind(this)
  }

  _handleMessage(message) {
    switch (message.data.type) {
      case "loadsf": {
        this.port.postMessage(`before sfload`)
        this._lib.FS.writeFile("/sf2.sf3", new Uint8Array(message.data.sfdata))

        // this.port.postMessage(`${ this.createStringPtr("/sf.sf2") }`)
        // this._lib._fluid_synth_sfload(this.synth, this.createStringPtr("/sf2.sf2"), 1)
        let sfloaded = this._lib.ccall('fluid_synth_sfload', 'undefined',
          ['number', 'string'], [this.synth, "/sf2.sf3"])
        this._lib._fluid_synth_program_select(this.synth, 0, 1, 0, 0);
        this.port.postMessage(`sfloaded? ${sfloaded}`)
        this.port.postMessage(`after sfload`)
        // this._buffer._refreshView()
        this.ready = true
        break;
      }
      case "on": {
        this.di = 0

        this.port.postMessage(`this buff is ${this._lib._fluid_web_get_chan_buff(this.buff, 1)}`)
        this._lib._fluid_web_noteon(this.synth, 2, 60, 127)
        this.port.postMessage(`did a note on`)
        break;
      }
      case "off": {
        this._lib._fluid_synth_noteoff(this.synth, 2, 60, 127)
        this.port.postMessage(`did a note off`)
        break;
      }
      case "here": {
        this.port.postMessage("here")
        break;
      }
      default: this.port.postMessage(`default trig`)

    }
  }

  process(input, output) {
    if (!this.ready) return
    const outputs = output[0]

    const writeresult = this._lib._fluid_web_process(this.synth, this.buff)

    if (writeresult == 0) {
      // outputs[0].set(this._lib.HEAPF32.subarray(r_i, r_i+128))
      // outputs[1].set(this._lib.HEAPF32.subarray(r_i, r_i+128))
      // outputs[1].set(this._lib.HEAPF32.subarray(this.buff/4, this.buff/4+128))
      // outputs[1].set(this._lib.HEAPF32.subarray(this.bu/2,this.buff/2 +128 ))
      // for (let i=0;i<128;i++) {
      //   outputs[0][i] = this._lib.HEAPF32[(r_ptr + i * 4) / 4]
      //   outputs[1][i] = this._lib.HEAPF32[(r_ptr + i * 4) / 4]
      // }

      outputs[0].set(this._lib.HEAPF32.subarray(this.buff / 4, this.buff / 4 + 128))
      outputs[1].set(this._lib.HEAPF32.subarray(this.buff / 4 + 128, this.buff / 4 + 128 * 2))
    } else {
      this.port.postMessage(writeresult)
    }


    return true
  }

})
