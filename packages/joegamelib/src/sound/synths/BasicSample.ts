// import * as Tone from 'tone'
import loadAfterLoad from 'utils/loadAfterLoad';
import { ITonerSynth } from '../ITonerSynth';

export interface TonerSynthConfig {
    assetURL: string
    id: string
    sampleN: number
    freqMap:  number[]
    volume: number
    duration?: number
}

export default class implements ITonerSynth {
    id: string
    volume: number
    ready: boolean = false
    sampleN: number
    freqMap: number[]
    duration: number

    constructor(private scene: Phaser.Scene, config: TonerSynthConfig) {
        this.id = config.id
        this.volume = config.volume
        this.sampleN = config.sampleN
        this.freqMap = config.freqMap
        this.duration = config.duration ?? 1
        this.init(config.assetURL)
    }

    play() {
        if (this.ready) {
            const rf =  this.getRandomFreq()
            this.scene.sound.play(this.id, {
                name: this.id+Math.random().toString(),
                start: Math.floor(Math.random() * this.sampleN),
                duration: this.duration,
                config: {
                    volume: this.volume,
                    rate: rf,
                }
            })
        }
    }

    setVolume() { }

    getRandomFreq() {
        const ri = Math.floor(Math.random()*this.freqMap.length)
        return this.freqMap[ri]
    }

    async init(assetURL: string) {
        await loadAfterLoad(this.scene, this.id, assetURL, 'audio')
        this.ready = true
    }

}
