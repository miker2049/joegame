import {createWithFontAndMIDI, playMidi} from "./utils.mjs"

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
  const node = await createWithFontAndMIDI(context,
                                           "/synth-worklet.js",
                                           "/gravis.sf2",
                                           "/venture.mid")
  node.connect(context.destination)
  document.querySelector("#playbutton").addEventListener("click", () => {
    context.resume()
      playMidi(node)
  })
  document.querySelector("#stopbutton").addEventListener("click", () => {
    context.resume()
  })

  document.addEventListener('keydown', (ev) => {
    if (!ev.repeat) {
      switch (ev.key) {
        case 'a': {
            context.resume()
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
