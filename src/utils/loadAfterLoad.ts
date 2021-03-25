import 'phaser'

export default async function(scene: Phaser.Scene, key: string, url: string): Promise<string> {
    return new Promise((res, rej) => {
        scene.load.json(key, url)
        if (scene.cache.json.exists(key)) {
            res(key)
        }
        scene.load.once('filecomplete', (keyy) => {
            // console.log(keyy)
            if (keyy === key) {
                res(key)
            }
        })
        scene.load.once('loaderror', (file) => {
            if (file.key === key) {
                rej(file)
            }
        })
        scene.load.start()
    })
}
