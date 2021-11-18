import Fluid from '../libfluidlite'
import WASMAudioBuffer from './WASMAudioBuffer'
//
// https://stackoverflow.com/a/49448982

registerProcessor('fluid', class extends AudioWorkletProcessor {
  // ...
  constructor(args) {
    super()
    this._lib = Fluid()
    // this._buffR = this._lib._malloc( BYTES_PER_SAMPLE * 128)
    // this._buffL = this._lib._malloc( BYTES_PER_SAMPLE * 128)
    this._buffer = new WASMAudioBuffer(this._lib,128,2,2)
    // this._lib.HEAPF32.set(new Float32Array(128), this._buffL/4)
    // this._lib.HEAPF32.set(new Float32Array(128), this._buffR/4)
    const settings = this._lib._new_fluid_settings()
    this.synth = this._lib._new_fluid_synth(settings)
    this.port.onmessage = this._handleMessage.bind(this)
    this.port.onmessageerror = this._handleMessage.bind(this)
  }

  _handleMessage(message) {
    switch (message.data.type) {
      case "loadsf": {
        this.port.postMessage(`before sfload`)
        this._lib.FS.writeFile("/sf2.sf2", new Uint8Array(message.data.sfdata))

        // this.port.postMessage(`${ this.createStringPtr("/sf.sf2") }`)
        // this._lib._fluid_synth_sfload(this.synth, this.createStringPtr("/sf2.sf2"), 1)
        this._lib.ccall('fluid_synth_sfload', 'undefined',
                        ['number', 'string'], [this.synth, "/sf2.sf2"])
        this.port.postMessage(`after sfload`)
        this._buffer._refreshView()
        this.ready = true
        break;
      }
      case "on": {
        this.di =0
        this._lib._fluid_synth_noteon(this.synth, 2, 60, 127)
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
      default:  this.port.postMessage(`default trig`)

    }
  }

  process(input, output) {
    if (!this.ready) return
    const outputs = output[0]
    /** Generate a number of samples. This function expects two floating
     *  point buffers (left and right channel) that will be filled with
     *  samples.
     *
     *  \param synth The synthesizer
     *  \param len The number of samples to generate
     *  \param lout The sample buffer for the left channel
     *  \param loff The offset, in samples, in the left buffer where the writing pointer starts
     *  \param lincr The increment, in samples, of the writing pointer in the left buffer
     *  \param rout The sample buffer for the right channel
     *  \param roff The offset, in samples, in the right buffer where the writing pointer starts
     *  \param rincr The increment, in samples, of the writing pointer in the right buffer
     *  \returns 0 if no error occured, non-zero otherwise
     */
    const writeresult = this._lib._fluid_synth_write_float(
      this.synth,
      128,
      this._buffer.getChannelData(0).byteOffset, 0, 1,
      this._buffer.getChannelData(0).byteOffset, 0, 1)

    this.port.postMessage(this._buffer.getChannelData(1).slice(1,23))

    if(writeresult == 0){
      outputs[0].set(this._buffer.getChannelData(0))
      outputs[1].set(this._buffer.getChannelData(1))
    } else { this.port.postMessage(writeresult) }
    return true
  }
})
