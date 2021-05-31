import 'phaser'
import { IWikiData } from './utils/parseWikiData'
// import { AudioContext } from 'standardized-audio-context';
/**
 * This returns a full config you can pass into `new Phaser.Game(config)`
 * */
// TODO type for resolver
export default function createJoegameConfig(gdata: IWikiData | string, baseURL: string, res: Function): Phaser.Types.Core.GameConfig {
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
        // audio: {
        //     context: new AudioContext()
        // },
        scene: {
            preload() {
                // TODO properly ignore this in some typescript way
                const scenee = this as Phaser.Scene
                scenee.load.setBaseURL(baseURL)
                scenee.registry.set('loaderBaseURL', baseURL)
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
