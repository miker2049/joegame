import { __awaiter } from "tslib";
import JSSynth from 'js-synthesizer';
export default function (midiURL, sfURL) {
    return __awaiter(this, void 0, void 0, function* () {
        const midiBuff = yield fetch(midiURL);
        const sfontBuffer = yield fetch(sfURL);
        const context = new AudioContext();
        yield context.audioWorklet.addModule('/joegame/libfluidsynth-2.1.3.js');
        yield context.audioWorklet.addModule('/joegame/js-synthesizer.worklet.js');
        // Create the synthesizer instance for AudioWorkletNode
        const synth = new JSSynth.AudioWorkletNodeSynthesizer();
        synth.init(context.sampleRate);
        // You must create AudioWorkletNode before using other methods
        // (This is because the message port is not available until the
        // AudioWorkletNode is created)
        const audioNode = synth.createAudioNode(context);
        audioNode.connect(context.destination); // or another node...
        // After node creation, you can use Synthesizer methods
        yield synth.loadSFont(yield sfontBuffer.arrayBuffer());
        yield synth.addSMFDataToPlayer(yield midiBuff.arrayBuffer());
        return synth.playPlayer();
    });
}
//# sourceMappingURL=playMIDI.js.map