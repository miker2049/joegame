import ITonerSynth from "./ITonerSynth";
import SynthBeep from "./SynthBeep";

export default class Toner {
    private instruments: Map<string, ITonerSynth>
    constructor() {
        this.instruments = new Map()
        this.instruments.set('arp', new SynthBeep())
    }
    play(inst: string) {
        const found = this.instruments.get(inst)
        if (found) found.play()
    }
}
