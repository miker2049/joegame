import {createWithFontAndMIDI, playMidi, pauseMidi, stopMidi} from "../utils.mjs"

(async function() {
  const div = document.createElement('div')
  // const idtag = sanitize(midifile)
  const playbuttonid = `playbutton`
  const pausebuttonid = `pausebutton`
  const stopbuttonid = `stopbutton`
  div.innerHTML = `
<h2>testing!</h2>
  <button id=${playbuttonid}>Play</button>
  <button id=${pausebuttonid}>Pause</button>
  <button id=${stopbuttonid}>Stop</button>

`
  document.querySelector("#tester-container").appendChild(div)
  const context = new AudioContext()
  const node = await createWithFontAndMIDI(context,
                                           "/dist/synth-worklet.js",
                                           "/assets/gravis.sf2",
                                           "/assets/venture.mid")
  node.connect(context.destination)
  node.port.postMessage({type: 'getpresetnames'})
  document.querySelector("#playbutton").addEventListener("click", () => {
    context.resume()
      playMidi(node)
  })
  document.querySelector("#pausebutton").addEventListener("click", () => {
    context.resume()
      node.port.postMessage({type: 'seekmidi', msec: 16000})
      // pauseMidi(node)
  })
  document.querySelector("#stopbutton").addEventListener("click", () => {
    context.resume()
      stopMidi(node)
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
