// import * as Tone from 'tone'
import loadAfterLoad from 'utils/loadAfterLoad';
import { ITonerSynth } from '../ITonerSynth';
import BasicSample from './BasicSample';

export default class extends BasicSample {
    constructor(scene: Phaser.Scene, assetURL: string) {
        super(scene,assetURL, 3, [1])
    }
}
