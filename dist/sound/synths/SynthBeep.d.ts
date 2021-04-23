import { ITonerSynth } from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string;
    volume: number;
    constructor();
    play(): void;
    setVolume(): void;
}
