import 'phaser';
/**
 * This returns a full config you can pass into `new Phaser.Game(config)`
 * */
// TODO type for resolver
export default function createJoegameConfig(gdata, res) {
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
        //     context: new Tone.Context()
        // },
        scene: {
            preload() {
                // TODO properly ignore this in some typescript way
                const scenee = this;
                scenee.load.json('gdata', gdata);
                // rawmap
                // this.load.json(getMapKeyNameRaw(data.mapjson),data.mapjson)
            },
            create() {
                // loadLevel(this, data)
                const scenee = this;
                res(scenee.game);
            },
            key: 'GameInitScene'
        }
    };
}
//# sourceMappingURL=gameconfig.js.map