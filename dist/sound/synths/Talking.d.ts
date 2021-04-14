import * as Tone from 'tone';
import ITonerSynth from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string;
    synths: Tone.Player[];
    panner: Tone.Panner;
    buffs: Tone.ToneAudioBuffer[];
    volume: number;
    ready: boolean;
    currSynth: number;
    constructor();
    play(vol?: number, pan?: number): void;
    setVolume(): void;
    private createBuff;
    init(): Promise<void>;
    private vowelUgen;
}
