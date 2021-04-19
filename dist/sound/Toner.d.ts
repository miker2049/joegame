import { IToner, ITonerPlayConfig } from './IToner';
import { ITonerSynth } from "./ITonerSynth";
export default class Toner implements IToner {
    instruments: Map<string, ITonerSynth>;
    context: AudioContext;
    constructor(context: AudioContext);
    play(config: ITonerPlayConfig): void;
}
