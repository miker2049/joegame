import JSSynth from 'js-synthesizer'

export default async function(midiURL: string, sfURL: string) {
    const midiBuff = await fetch(midiURL)
    const sfontBuffer = await fetch(sfURL)
    const context = new AudioContext()
    await context.audioWorklet.addModule('/joegame/libfluidsynth-2.1.3.js')
    await context.audioWorklet.addModule('/joegame/js-synthesizer.worklet.js')
    // Create the synthesizer instance for AudioWorkletNode
    const synth = new JSSynth.AudioWorkletNodeSynthesizer()
    synth.init(context.sampleRate)
    // You must create AudioWorkletNode before using other methods
    // (This is because the message port is not available until the
    // AudioWorkletNode is created)
    const audioNode = synth.createAudioNode(context)
    audioNode.connect(context.destination) // or another node...
    // After node creation, you can use Synthesizer methods
    await synth.loadSFont(await sfontBuffer.arrayBuffer())
    await synth.addSMFDataToPlayer(await midiBuff.arrayBuffer())
    return synth.playPlayer()
}
