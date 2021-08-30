import 'phaser'
/**
 * This returns a full config you can pass into `new Phaser.Game(config)`
 * */
// TODO type for resolver
    const config = {
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
        }

    }
export default config
