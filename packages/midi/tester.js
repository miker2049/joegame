
export async function createSynth(acontext, workletURL) {
    try {
        await acontext.audioWorklet.addModule(workletURL)
    } catch (err) {
        console.log(err)
    }
    const node = new AudioWorkletNode(acontext, 'synth', {
        outputChannelCount: [2],
        processorOptions: {}
    })

    return await new Promise((res, rej) => {
        node.port.onmessage = (msg) => {
            console.log(msg)
            if(msg.data === 'INITIALIZED')
                res(node)
        }
        // setTimeout(()=>res(node), 1000)

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
  // console.log(mesg.data)
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
    const node = await createSynth(context, "/synth-worklet.js")
  node.port.onmessage = handlemsg.bind(this)
  node.port.onmessageerror = handlemsg.bind(this)
  const sffile = await (await fetch("/florestan-subset.sf2")).arrayBuffer()
  const arr  = new Uint8Array(sffile)
  node.port.postMessage({ type: "loadsf", sfdata: arr, isogg: 0 })
  document.querySelector("#playbutton").addEventListener("click", () => {
    context.resume()
    node.port.postMessage({ type: "on" })
    setTimeout(() => {
      node.port.postMessage({ type: "off" })
    }, 1000)
  })
  document.querySelector("#stopbutton").addEventListener("click", () => {
    context.resume()
    node.port.postMessage({ type: "off" })

    // node.port.postMessage({ type: "on" })
    // setTimeout(()=>{
    //   node.port.postMessage({ type: "off" })
    // }, 1000)
  })
  node.connect(context.destination)
  document.addEventListener('keydown', (ev) => {
    if (!ev.repeat) {
      switch (ev.key) {
        case 'a': {
          node.port.postMessage({ type: 'on', note: 60 })
          break;
        }
        case 's': {
          node.port.postMessage({ type: 'on', note: 62 })
          break;
        }
        case 'd': {
          node.port.postMessage({ type: 'on', note: 64 })
          break;
        }
        case 'f': {
          node.port.postMessage({ type: 'on', note: 66 })
          break;
        }
      }

    }
  })
  document.addEventListener('keyup', (ev) => {
    switch (ev.key) {
      case 'a': {
        node.port.postMessage({ type: 'off', note: 60 })
        break;
      }
      case 's': {
        node.port.postMessage({ type: 'off', note: 62 })
        break;
      }
      case 'd': {
        node.port.postMessage({ type: 'off', note: 64 })
        break;
      }
      case 'f': {
        node.port.postMessage({ type: 'off', note: 66 })
        break;
      }
    }
  })

})()
