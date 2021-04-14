import * as Tone from 'tone'
import ITonerSynth from "./ITonerSynth";
import Gong from "./synths/Gong";
import SynthBeep from "./synths/SynthBeep";
import Talking from "./synths/Talking";
import Walk from "./synths/Walk";

export default class Toner {
    private instruments: Map<string, ITonerSynth>
    private context: AudioContext
    constructor(context: AudioContext) {
        this.context = context
        Tone.setContext(this.context)
        this.instruments = new Map()
        this.instruments.set('arp', new SynthBeep())
        this.instruments.set('gong', new Gong())
        this.instruments.set('walk', new Walk())
        Tone.setContext(this.context)
        this.instruments.set('talking', new Talking())
        Tone.setContext(this.context)
        Tone.start()
        Tone.Transport.start()
    }
    play(inst: string) {
        const found = this.instruments.get(inst)
        if (found) found.play()
    }
}
