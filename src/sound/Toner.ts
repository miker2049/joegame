import ITonerSynth from "./ITonerSynth";
import Gong from "./synths/Gong";
import SynthBeep from "./synths/SynthBeep";
import Walk from "./synths/Walk";

export default class Toner {
    private instruments: Map<string, ITonerSynth>
    constructor() {
        this.instruments = new Map()
        this.instruments.set('arp', new SynthBeep())
        this.instruments.set('gong', new Gong())
        this.instruments.set('walk', new Walk())
    }
    play(inst: string) {
        const found = this.instruments.get(inst)
        if (found) found.play()
    }
}
