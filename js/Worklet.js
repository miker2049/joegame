import Fluid from '../libfluidlite'
//
// https://stackoverflow.com/a/49448982
function cArray(size, module) {
    var offset = module._malloc(size * 4);
    module.HEAPF32.set(new Float32Array(size), offset / 4);
    return {
        "data": module.HEAPF64.subarray(offset / 4, offset / 4 + size),
        "offset": offset
    }
}

registerProcessor('fluid', class extends AudioWorkletProcessor {
  // ...
  constructor(args) {
    super()
    this._lib = Fluid()
    this._buffR = this._lib._malloc(128)
    this._buffL = this._lib._malloc(128)
    const settings = this._lib._new_fluid_settings()
    this.synth = this._lib._new_fluid_synth(settings)
    this.port.onmessage = this._handleMessage.bind(this)
    this.port.onmessageerror = this._handleMessage.bind(this)
    this.di = 0
  }
  async init() {
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
  createStringPtr(str){

    const len = this._lib.lengthBytesUTF8(str)
    const ptr = this._lib._malloc(len)
    this._lib.stringToUTF8(str,ptr)
    return ptr
  }
  process(input, output) {
    if (!this.ready) return
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
    this.di += 1
    const writeresult = this._lib._fluid_synth_write_float(this.synth, 128, this._buffL, 0, 1, this._buffR, 0, 1)
    if(writeresult == 0){
      // const bl = this._lib.HEAPF32.subarray(this._buffL/2, (this._buffL/2) + 128 )
      // const br = this._lib.HEAPF32.subarray(this._buffR/2, (this._buffR/2) + 128 )
      // output[0][0].set(bl, 0)
      // output[0][1].set(br, 0)
      for(let i =0; i<128; i++){
        output[0][0][i] = this._lib.HEAPF32[i+(this._buffL/4)]
        output[0][1][i] = this._lib.HEAPF32[i+(this._buffR/4)]
      // output[0][1][i]
      }
    } else { this.port.postMessage(writeresult) }
    // output[0][1] = this._lib.HEAPF32.slice(this._buffR, this._buffR + (128*8))
    return true
  }
})
