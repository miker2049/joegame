// import * as Tone from 'tone'
import { ILevelComponents } from '../ILevel'
import wikiData from '../utils/wikiData'
import { IToner, ITonerPlayConfig, joegameSounds } from './IToner'
// import Gong from "./synths/Gong";
// import SynthBeep from "./synths/SynthBeep";
// import { Talking } from "./synths/Talking";
import defaults from '../defaults'
import BasicSample from './synths/BasicSample'
import musicalScales from '../utils/musicalScales'

export default class Toner implements IToner {
  instruments: joegameSounds

  constructor(level: ILevelComponents) {
    const url = (
      wikiData(level.scene.game).sound.get('walk') ?? { url: defaults.soundURL }
    ).url
    this.instruments = {
      walk: new BasicSample(level.scene, {
        assetURL: getSomeSoundURL(level.scene, 'walk'),
        id: 'walk',
        volume: 0.2,
        sampleN: 3,
        freqMap: [1]
      }),
      itemPickup: new BasicSample(level.scene, {
        assetURL: getSomeSoundURL(level.scene, 'item'),
        id: 'item',
        volume: 1,
        sampleN: 3,
        duration: 0.8,
        freqMap: musicalScales.itemScale
      })
      // talk: new Walk(level.scene, url),
    }
  }
  play(config: ITonerPlayConfig) {
    const found = this.instruments[config.inst]
    if (found) found.play(config)
  }
}

function getSomeSoundURL(scene: Phaser.Scene, id: string) {
  let f = wikiData(scene.game).sound.get(id)
  if (f) {
    return f.url
  } else {
    console.warn(
      `The sound with id ${id} is not found in wikidata (using default sound)`
    )
    return defaults.soundURL
  }
}
