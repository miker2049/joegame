import Phaser from "phaser"

export async function getTestScene() {

    return new Promise<Phaser.Scene>((res, rej) => {
        new Phaser.Game({
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
            scene: {
                preload() {
                },
                create() {
                    const scenee = this as Phaser.Scene
                    res(scenee)
                },
                key: 'testScene'
            }
        }
        )
    }
    )
}
