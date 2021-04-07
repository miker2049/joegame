import { ToneAudioBuffer, Player } from 'tone';
import ITonerSynth from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string;
    synth: Player;
    buff: ToneAudioBuffer | undefined;
    volume: number;
    ready: boolean;
    constructor();
    play(): void;
    setVolume(): void;
}
