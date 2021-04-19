import * as Tone from 'tone';
import { ITonerSynth } from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string;
    synth: Tone.PolySynth;
    volume: number;
    constructor();
    play(): void;
    setVolume(): void;
}
