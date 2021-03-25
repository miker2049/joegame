import "phaser";
import { IWikiData, parsewikidata } from './utils/parseWikiData';
// import { loadLevel } from "./levelLoader"
// const wikidata = require('./wikidata.json')

import RainfallPostFX from './shaders/RainfallPostFX'
import PlasmaPostFX from './shaders/PlasmaPostFX'
import Blob from './shaders/Blobs'
import Clouds from './shaders/clouds'
// export interface InitDataJgame{
//     mapjson: string
//     x: number
//     y: number
//     callbacks?:Function[]
//     startLevel?: boolean
// }

/**
 * This returns a full config you can pass into `new Phaser.Game(config)`
 * */
// TODO type for resolver
export default function createJoegameConfig(gdata: IWikiData | string, convoManifest: any[] | string, res: Function): Phaser.Types.Core.GameConfig {
    return {
        type: Phaser.WEBGL,
        backgroundColor: "#0d0c0a",
        render: {
            pixelArt: true,
            pipeline: {
                RainfallPostFX,
                PlasmaPostFX,
                Blob,
                Clouds
            },
        },
        scale: {
            mode: Phaser.Scale.FIT,
            autoCenter: Phaser.Scale.CENTER_BOTH,
            width: 800,
            height: 600,
        },
        parent: "frame",
        dom: {
            createContainer: true
        },
        physics: {
            default: 'arcade',
            arcade: {
                gravity: { y: 0 },
                debug: false

            },
        },
        scene: {
            preload() {
                // TODO properly ignore this in some typescript way
                this.load.json('gdata', gdata)
                this.load.json('convo-manifest', convoManifest)
                //rawmap
                // this.load.json(getMapKeyNameRaw(data.mapjson),data.mapjson)
            },
            create() {
                // loadLevel(this, data)
                res(this.game)
            },
            key: 'GameInitScene'
        },

    }
}
