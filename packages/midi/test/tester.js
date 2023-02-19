import MIDIClient from "/dist/midi.js";
(async function () {
    const div = document.createElement("div");
    // const idtag = sanitize(midifile)
    const playbuttonid = `playbutton`;
    const pausebuttonid = `pausebutton`;
    const stopbuttonid = `stopbutton`;
    div.innerHTML = `
<h2>testing!</h2>
  <button id=${playbuttonid}>Play</button>
  <button id=${pausebuttonid}>Pause</button>
  <button id=${stopbuttonid}>Stop</button>

`;
    document.querySelector("#tester-container").appendChild(div);
    const context = new AudioContext();
    const client = await MIDIClient.createWithFontAndMIDI(
        context,
        "/dist/synth-worklet.js",
        "/assets/gravis.sf3",
        "/assets/bach.mid"
    );
    client.node.connect(context.destination);
    document.querySelector("#playbutton").addEventListener("click", () => {
        client.playMidi();
        context.resume();
        client.noteon(60, 1, 1);
    });
    document.querySelector("#pausebutton").addEventListener("click", () => {
        context.resume();
        client.pauseMidi();
    });
    document.querySelector("#stopbutton").addEventListener("click", () => {
        context.resume();
        client.stopMidi();
    });
})();
