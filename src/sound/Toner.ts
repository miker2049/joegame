// import * as Tone from 'tone'
import { ILevelComponents } from 'ILevel';
import wikiData from 'utils/wikiData';
import { IToner, ITonerPlayConfig, joegameSounds } from './IToner';
import { ITonerSynth } from "./ITonerSynth";
// import Gong from "./synths/Gong";
// import SynthBeep from "./synths/SynthBeep";
// import { Talking } from "./synths/Talking";
import Walk from "./synths/Walk";
import defaults from '../defaults'


export default class Toner implements IToner {
    instruments: joegameSounds

    constructor(level: ILevelComponents) {

        const url = (wikiData(level.scene.game).sound.get('walk') ?? { url: defaults.soundURL }).url
        this.instruments = {
            walk: new Walk(level.scene, url),
            talk: new Walk(level.scene, url),
        }
    }
    play(config: ITonerPlayConfig) {
        const found = this.instruments[config.inst]
        if (found) found.play(config)
    }
}
