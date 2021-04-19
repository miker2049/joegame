import * as Phaser from 'phaser'
import { createAudioContext } from 'tone/build/esm/core/context/AudioContext'
import { IWikiData } from './utils/parseWikiData'

/**
 * This returns a full config you can pass into `new Phaser.Game(config)`
 * */
// TODO type for resolver
export default function createJoegameConfig(gdata: IWikiData | string, res: Function): Phaser.Types.Core.GameConfig {
    return {
        type: Phaser.WEBGL,
        render: {
            pixelArt: true
        },
        scale: {
            mode: Phaser.Scale.FIT,
            autoCenter: Phaser.Scale.CENTER_BOTH,
            width: 800,
            height: 600
        },
        parent: 'frame',
        dom: {
            createContainer: true
        },
        physics: {
            default: 'arcade',
            arcade: {
                gravity: { y: 0 },
                debug: false

            }
        },
        audio: {
            context: createAudioContext()
        },
        scene: {
            preload() {
                // TODO properly ignore this in some typescript way
                const scenee = this as Phaser.Scene
                scenee.load.json('gdata', gdata)
                // rawmap
                // this.load.json(getMapKeyNameRaw(data.mapjson),data.mapjson)
            },
            create() {
                // loadLevel(this, data)
                const scenee = this as Phaser.Scene
                res(scenee.game)
            },
            key: 'GameInitScene'
        }

    }
}
