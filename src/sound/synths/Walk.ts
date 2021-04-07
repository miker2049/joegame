import { PluckSynth, now, Offline, ToneAudioBuffer, Player, loaded } from 'tone'
import ITonerSynth from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string = 'walk'
    synth: Player
    buff: ToneAudioBuffer | undefined
    volume: number = 0.75
    ready: boolean = false
    constructor() {
        Offline((transport) => {
            const pluck = new PluckSynth({ resonance: 0.1, volume: -12 }).toDestination();
            const tnow = transport.currentTime
            pluck.triggerAttack("A3", tnow);
            pluck.triggerAttack("G3", tnow + (0.125 / 4));
        }, 0.25).then(buff => {
            this.ready = true
            this.synth = new Player(buff).toDestination()
        })
    }
    play() {
        if (this.ready) {
            console.log('walk')
            this.synth.start();
        }
    }
    setVolume() { }
}
