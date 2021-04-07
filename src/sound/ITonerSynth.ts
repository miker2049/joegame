import { Synth, Player, PluckSynth } from 'tone'
export default interface ITonerSynth {
    id: string
    synth: Synth | Player | PluckSynth
    volume: number
    setVolume(vol: number): void
    play(): void
}
