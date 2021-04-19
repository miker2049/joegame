import * as Tone from 'tone';
import { ITonerSynth } from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string;
    synth: Tone.Player;
    buff: Tone.ToneAudioBuffer | undefined;
    volume: number;
    ready: boolean;
    constructor();
    play(): void;
    setVolume(): void;
}
