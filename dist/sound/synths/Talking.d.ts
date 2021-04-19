import { ITonerPlayConfig } from 'sound/IToner';
import * as Tone from 'tone';
import { ITonerSynth } from '../ITonerSynth';
export interface ITalkingPlayConfig extends ITonerPlayConfig {
    inst: 'talking';
    buff: number;
    rate: number;
}
export declare class Talking implements ITonerSynth {
    id: string;
    synths: Tone.Player[];
    panner: Tone.Panner;
    buffs: Tone.ToneAudioBuffer[];
    volume: number;
    ready: boolean;
    currSynth: number;
    constructor();
    play(config: ITalkingPlayConfig): void;
    setVolume(): void;
    private createBuff;
    init(): Promise<void>;
    private vowelUgen;
}
