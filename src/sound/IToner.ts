import { ITonerSynth } from "./ITonerSynth"

export interface IToner {
    instruments: joegameSounds
    play(config: ITonerPlayConfig): void
}

export interface ITonerPlayConfig {
    inst: string
    pan?: number // -1.0 to 1.0
    vol?: number // 0.0 to 1.0
}

export interface joegameSounds {
    walk: ITonerSynth
    talk: ITonerSynth
}
