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
    this.port.onmessage = this._handleMessage.bind(this)
    this.port.onmessageerror = this._handleMessage.bind(this)
    this._qu = []
  }

  _handleMessage(message) {
    switch (message.data.type) {
      case "loadsf": {
        this.port.postMessage(`before sfload`)
        // this._synth = this._lib._init_web(message.data.sfdata.by)
        const sfptr = this._lib._malloc(message.data.sfdata.byteLength);
        this._lib.HEAPU8.set(message.data.sfdata, sfptr)
        this._synth = this._lib.ccall('init_web',
                                      'number',
                                      ['number', 'number'],
                                      [sfptr, message.data.sfdata.byteLength])
        // const voicres = this._lib._tsf_set_max_voices(this._synth, 128)
        this.port.postMessage(`after sfload, data ${message.data.sfdata.byteLength}`)
        this.port.postMessage(`after sfload, buff pointer is ${this._buff}`)
        this.port.postMessage(`after sfload, synth pointer is ${this._synth}`)
        this.ready = true
        break;
      }
      case "on": {
        let note = message.data.note ?? 60
        this._lib._noteon_web(this._synth, 0, note, 1)
        // this._qu.push([0,0,48,1])
        //
        this.port.postMessage(`voice count ${this._lib._tsf_active_voice_count(this._synth)}`)
        // this.port.postMessage(`did a note on ${res}`)
        this.port.postMessage(`preset count ${this._lib._tsf_get_presetcount(this._synth)}`)
        break;
      }
      case "off": {
        let note = message.data.note ?? 60
        // this._qu.push([1,0,48,1])
        this._lib._noteoff_web(this._synth, 0, note, 1)
        this.port.postMessage(`did a note off`)
        this.port.postMessage(`voice count ${this._lib._tsf_active_voice_count(this._synth)}`)
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
    const buff = this._lib._process_web(this._synth)
    this._qu.forEach(ev=>{
      if(ev[0]===0){
        this._lib._noteon_web(ev[1],ev[2],ev[3])
      } else if(ev[0]===1){
        this._lib._noteoff_web(ev[1],ev[2],ev[3])
      }
    });
    this._qu = []
    // const arr = this._lib.HEAPF32.subarray(buff/4,buff/4+128)
    outputs[0].set( this._lib.HEAPF32.subarray(buff/4,buff/4+128) )
    outputs[1].set( this._lib.HEAPF32.subarray(buff/4+128,buff/4+128*2) )
    // for (var i = 0; i < 128; ++i) {
    //   outputs[0][i] = arr[i]
    //   outputs[1][i] = arr[i]
    // }
    return true
  }
})
