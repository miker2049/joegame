import { __awaiter } from "tslib";
import * as Tone from 'tone';
const envelope = {
    attack: 0.01,
    sustain: 1.0,
    decay: 0.01
};
const root = 261.63; // G4
const fourth = 349.23;
const third = 329.63;
const fifth = 392.;
const sixth = 440.;
const maj7 = 493.88;
const oct = 523.25;
// const intervals = [root, fifth, fifth, third, maj7, fourth, sixth, oct]
const intervals = [root, fourth];
const VOICES = 8;
export class Talking {
    constructor() {
        this.id = 'talking';
        this.volume = 0.75;
        this.ready = false;
        this.currSynth = 0;
        this.buffs = [];
        this.synths = [];
        for (let i = 0; i < VOICES; i++) {
            this.synths.push((() => {
                const panner = new Tone.Panner(0).toDestination();
                return {
                    player: new Tone.Player().connect(panner),
                    panner: panner
                };
            })());
        }
        this.init().then(() => this.ready = true);
    }
    play(config) {
        var _a, _b;
        if (this.ready) {
            // if (config.pan) this.panner.set({ pan: config.pan })
            // this.panner.set({ pan: (Math.random() * 2) - 1 })
            this.synths[this.currSynth].panner.set({ pan: config.pan });
            this.synths[this.currSynth].player.volume.rampTo(((_a = config.vol) !== null && _a !== void 0 ? _a : 0.5) * -18);
            const buffer = (_b = this.buffs[config.buff % this.buffs.length]) !== null && _b !== void 0 ? _b : this.buffs[Math.floor(Math.random() * this.buffs.length)];
            this.synths[this.currSynth].player.buffer = buffer;
            const interval = config.rate ? (intervals[config.rate % intervals.length] / root) : (intervals[Math.floor(Math.random() * intervals.length)] / root);
            this.synths[this.currSynth].player.playbackRate = interval;
            // console.log(config.rate, buffer, interval);
            this.synths[this.currSynth].player.start(Tone.now() + 0.01);
            this.currSynth = (this.currSynth + 1) % VOICES;
        }
    }
    setVolume() { }
    createBuff(cb) {
        return __awaiter(this, void 0, void 0, function* () {
            return yield Tone.Offline(cb, 0.25);
        });
    }
    init() {
        return __awaiter(this, void 0, void 0, function* () {
            // https://soundbridge.io/formants-vowel-sounds/
            const vowels = [
                [570, 840, 2410, 60],
                [300, 870, 2240, 60],
                [440, 1020, 2240, 60],
                [730, 1090, 2440, 60],
                [520, 1190, 2390, 60],
                [490, 1350, 1690, 60],
                [660, 1720, 2410, 60],
                [530, 1840, 2480, 60],
                [390, 1990, 2550, 60],
                [270, 2290, 3010, 60]
            ];
            Promise.all(vowels.map((args) => {
                return this.createBuff((_transport) => { this.vowelUgen(args[0], args[1], args[2], Tone.Frequency(args[3], "midi").toFrequency()); });
            })).then(buffs => this.buffs = buffs);
        });
    }
    vowelUgen(f1, f2, f3, pitch) {
        // https://github.com/benfordslaw/vowel-sound-generator/blob/main/sketch.js
        const vol = new Tone.Volume(0).toDestination();
        const filtf1 = new Tone.Filter(f1, "bandpass").connect(vol);
        filtf1.Q.value = 5.0;
        const filtf2 = new Tone.Filter(f2, "bandpass").connect(vol);
        filtf2.Q.value = 13.0;
        const filtf3 = new Tone.Filter(f3, "bandpass").connect(vol);
        filtf3.Q.value = 14.0;
        const ampEnv = new Tone.AmplitudeEnvelope({
            attack: 0.1,
            decay: 0.2,
            sustain: 1.0,
            release: 0.8
        }).fan(filtf1, filtf2, filtf3);
        //noise adds a little natural-ness
        // const noi = new Tone.Noise({
        //     type: "pink",
        //     volume: -16
        // }).connect(ampEnv).start()
        const osc = new Tone.Oscillator({
            type: "sawtooth",
            frequency: pitch,
            volume: -8
        }).connect(ampEnv).start();
        ampEnv.triggerAttackRelease("8t");
    }
}
//# sourceMappingURL=Talking.js.map