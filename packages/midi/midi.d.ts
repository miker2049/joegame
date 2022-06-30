declare module 'midi' {
    export function createSynth(ctx: AudioContext, workletURL: string): Promise<AudioWorkletNode>
    export function loadFont(node: AudioWorkletNode, sfURL: string): Promise<void>
}
