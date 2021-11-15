// import raw from "../Fluidlite/libfluidlite.wasm"
import raw from "../zig-out/lib/fluidlite.wasm"
const typedArray = new Uint8Array(raw);

WebAssembly.instantiate(raw, {
  //importts
  env: {
  },
  module: {}
}).then(instance => {
  const exports = instance.exports
  // console.log(exports.add())
})
