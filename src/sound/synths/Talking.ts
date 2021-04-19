import { ITonerPlayConfig } from 'sound/IToner';
import * as Tone from 'tone'
import { ToneAudioBuffer } from 'tone';
import { ITonerSynth } from '../ITonerSynth';

const envelope = {
    attack: 0.01,
    sustain: 1.0,
    decay: 0.01
}
const root = 261.63 // G4

const fourth = 349.23
const third = 329.63
const fifth = 392
const maj7 = 493.88

const intervals = [root, fifth, fifth, third, maj7]

const VOICES = 8

export interface ITalkingPlayConfig extends ITonerPlayConfig {
    inst: 'talking'
    buff: number
    rate: number
}

export class Talking implements ITonerSynth {
    id: string = 'talking'
    synths: Tone.Player[]
    panner: Tone.Panner
    buffs: Tone.ToneAudioBuffer[]
    volume: number = 0.75
    ready: boolean = false
    currSynth: number = 0
    constructor() {
        this.buffs = []
        this.panner = new Tone.Panner(0).toDestination()
        this.synths = []
        for (let i = 0; i < VOICES; i++) {
            this.synths.push(new Tone.Player({ fadeIn: 0.005, fadeOut: 0.005 }).connect(this.panner))
        }
        this.init().then(() => this.ready = true)

    }

    play(config: ITalkingPlayConfig) {
        if (this.ready) {
            if (config.pan) this.panner.set({ pan: config.pan })
            // this.panner.set({ pan: (Math.random() * 2) - 1 })
            this.synths[this.currSynth].volume.value = config.vol ?? -12
            const buffer = this.buffs[config.buff % this.buffs.length] ?? this.buffs[Math.floor(Math.random() * this.buffs.length)]
            this.synths[this.currSynth].buffer = buffer
            const interval = config.rate ? (intervals[config.rate % intervals.length] / root) : (intervals[Math.floor(Math.random() * intervals.length)] / root)
            this.synths[this.currSynth].playbackRate = interval
            console.log(config.rate, buffer, interval);
            this.synths[this.currSynth].start(Tone.now() + 0.01)
            this.currSynth = (this.currSynth + 1) % VOICES
        }
    }

    setVolume() { }

    private async createBuff(cb: (context: Tone.OfflineContext) => void): Promise<ToneAudioBuffer> {
        return await Tone.Offline(cb, 0.25)
    }

    async init() {
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
        ]
        Promise.all<ToneAudioBuffer>(vowels.map((args): Promise<ToneAudioBuffer> => {
            return this.createBuff((_transport) => { this.vowelUgen(args[0], args[1], args[2], Tone.Frequency(args[3], "midi").toFrequency()) })
        })).then(buffs => this.buffs = buffs)
    }

    private vowelUgen(f1: number, f2: number, f3: number, pitch: number) {
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
        const noi = new Tone.Noise({
            type: "pink",
            volume: -16
        }).connect(ampEnv).start()

        const osc = new Tone.Oscillator({
            type: "sawtooth",
            frequency: pitch,
            volume: -8
        }).connect(ampEnv).start()

        ampEnv.triggerAttackRelease("8t");
    }

}
