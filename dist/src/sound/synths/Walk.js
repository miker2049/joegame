import * as Tone from 'tone';
export default class {
    constructor() {
        this.id = 'walk';
        this.volume = 0.75;
        this.ready = false;
        this.synth = new Tone.Player().toDestination();
        Tone.Offline((transport) => {
            const pluck = new Tone.PluckSynth({ resonance: 0.1, volume: -18 }).toDestination();
            const tnow = transport.currentTime;
            pluck.triggerAttack("A3", tnow);
            pluck.triggerAttack("G3", tnow + (0.125 / 4));
        }, 0.25).then(buff => {
            this.ready = true;
            this.synth.buffer = buff;
        });
    }
    play() {
        if (this.ready) {
            // this.synth.stop()
            this.synth.start();
        }
    }
    setVolume() { }
}
//# sourceMappingURL=Walk.js.map