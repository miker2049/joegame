import { Synth } from 'tone'
export default interface ITonerSynth {
    id: string
    synth: Synth
    volume: number
    setVolume(vol: number): void
    play(): void
}
