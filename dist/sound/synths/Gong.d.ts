import { Player } from 'tone';
import ITonerSynth from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string;
    synth: Player;
    volume: number;
    constructor();
    play(): void;
    setVolume(): void;
}
