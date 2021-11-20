
async function createSynth(acontext) {

  try {
    await acontext.audioWorklet.addModule("/out.js")
  } catch (err) {
    console.log(err)
  }

  return new AudioWorkletNode(acontext, 'synth', {
    outputChannelCount: [2],
    processorOptions: {}
  })

}



function createPlayerUI(midifile, container, context) {
  const parent = document.querySelector(container)
  const div = document.createElement('div')
  const idtag = sanitize(midifile)
  const loadbuttonid = `loadbutton-${idtag}`
  const playbuttonid = `playbutton-${idtag}`
  const stopbuttonid = `stopbutton-${idtag}`
  div.innerHTML = `
<h2>${midifile}</h2>
  <button id=${loadbuttonid}>Load</button>
  <button id=${playbuttonid}>Play</button>
  <button id=${stopbuttonid}>Pause</button>

`
  div.className = "test-container-" + idtag
  parent.appendChild(div)
  createSynth(context).then(node => {
    node.connect(context.destination)

  })

}

function handlemsg(mesg) {
  console.log(mesg.data)
}

(async function() {
  const div = document.createElement('div')
  // const idtag = sanitize(midifile)
  const loadbuttonid = `loadbutton`
  const playbuttonid = `playbutton`
  const stopbuttonid = `stopbutton`
  div.innerHTML = `
<h2>testing!</h2>
  <button id=${loadbuttonid}>Load</button>
  <button id=${playbuttonid}>Play</button>
  <button id=${stopbuttonid}>Pause</button>

`
  document.querySelector("#tester-container").appendChild(div)
  const context = new AudioContext()
  const node = await createSynth(context)
  node.port.onmessage = handlemsg.bind(this)
  node.port.onmessageerror = handlemsg.bind(this)
  const sffile = await (await fetch("/gravis.sf2")).arrayBuffer()
  node.port.postMessage({type: "loadsf", sfdata: sffile, size: sffile.byteLength})
  document.querySelector("#playbutton").addEventListener("click", () => {
    context.resume()
    node.port.postMessage({ type: "on" })
    setTimeout(()=>{
      node.port.postMessage({ type: "off" })
    }, 1000)
  })
  document.querySelector("#stopbutton").addEventListener("click", () => {
    context.resume()
    // node.port.postMessage({ type: "on" })
    // setTimeout(()=>{
    //   node.port.postMessage({ type: "off" })
    // }, 1000)
  })
  node.connect(context.destination)
  window.worklet = node
})()
