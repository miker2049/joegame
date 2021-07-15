// import * as Tone from 'tone'
import loadAfterLoad from 'utils/loadAfterLoad';
import { ITonerSynth } from '../ITonerSynth';
export default class implements ITonerSynth {
    id: string = 'walk'
    volume: number = 0.75
    ready: boolean = false

    constructor(private scene: Phaser.Scene, assetURL: string) {
        this.init(assetURL)
    }

    play() {
        if (this.ready) {
            // this.synth.stop()

            this.scene.sound.play(this.id, {
                name: this.id + "1",
                start: Math.floor(Math.random() * 3),
                duration: 1,
                config: {
                    volume: 0.5
                }
            })
        }
    }

    setVolume() { }

    async init(assetURL: string) {
        await loadAfterLoad(this.scene, this.id, assetURL, 'audio')
        this.ready = true
    }

}
