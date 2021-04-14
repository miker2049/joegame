export default interface ITonerSynth {
    id: string
    volume: number
    setVolume(vol: number): void
    play(pan?: number): void
}
