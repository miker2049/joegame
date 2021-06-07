import 'phaser'

export default async function(
    scene: Phaser.Scene,
    key: string,
    url: string,
    loader: string): Promise<string> {

    return new Promise(function(res, rej) {
        const file = new Phaser.Loader.File(scene.load, { key: key, url: url, type: loader })
        if (scene.load.keyExists(file)) res(key)

        scene.load[loader](key, url)

        scene.load.on('filecomplete', (keyy: string, path: string) => {
            if (keyy === key) {
                res(key)
            }
        })
        scene.load.on('loaderror', (file: { key: string }) => {
            if (file.key === key) {
                rej(file)
            }
        })
        scene.load.start()
    })
}
