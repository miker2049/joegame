import { ITonerSynth } from "./ITonerSynth";
export interface IToner {
    instruments: Map<string, ITonerSynth>;
    context: AudioContext;
    play(config: ITonerPlayConfig): void;
}
export interface ITonerPlayConfig {
    inst: string;
    pan?: number;
    vol?: number;
}
