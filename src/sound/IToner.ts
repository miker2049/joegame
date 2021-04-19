import { ITonerSynth } from "./ITonerSynth"

export interface IToner {

    instruments: Map<string, ITonerSynth>
    context: AudioContext
    play(config: ITonerPlayConfig): void
}

export interface ITonerPlayConfig {
    inst: string
    pan?: number // -1.0 to 1.0
    vol?: number // 0.0 to 1.0
}
