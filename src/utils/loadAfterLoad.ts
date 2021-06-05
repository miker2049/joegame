import 'phaser'

export default async function(
    scene: Phaser.Scene,
    key: string,
    url: string,
    loader: string) {

    return new Promise(function(res, rej) {
        scene.load[loader](key, url, loader)
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
