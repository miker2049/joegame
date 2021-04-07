import { Synth, now } from 'tone'
import ITonerSynth from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string = 'Arp'
    synth: Synth
    volume: number = 0.75
    constructor() {
        this.synth = new Synth().toDestination();
    }
    play() {
        const tnow = now()
        this.synth.triggerAttackRelease("C4", "8n", tnow)
        this.synth.triggerAttackRelease("E4", "8n", tnow + 0.5)
        this.synth.triggerAttackRelease("G4", "8n", tnow + 1)
        this.synth.triggerAttackRelease("B4", "8n", tnow + 1.5)
    }
    setVolume() { }
}
