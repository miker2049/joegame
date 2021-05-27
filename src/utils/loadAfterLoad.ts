import 'phaser'

export default async function(
    scene: Phaser.Scene,
    key: string,
    url: string,
    loader: string) {

    // console.log(loader)

    return new Promise(function(res, rej) {
        scene.load[loader](key, url)
        scene.load.once('filecomplete', (keyy: string) => {
            // console.log(keyy)
            if (keyy === key) {
                res(key)
            }
        })
        scene.load.once('loaderror', (file: { key: string }) => {
            if (file.key === key) {
                rej(file)
            }
        })
        scene.load.start()
    })
}
