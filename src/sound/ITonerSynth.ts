import { ITonerPlayConfig } from "./IToner";

export interface ITonerSynth {
    id: string
    volume: number
    setVolume(vol: number): void
    play(config: ITonerPlayConfig): void
}
