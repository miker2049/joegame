import { Synth } from 'tone';
import ITonerSynth from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string;
    synth: Synth;
    volume: number;
    constructor();
    play(): void;
    setVolume(): void;
}
