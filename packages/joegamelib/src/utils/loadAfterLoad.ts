import 'phaser'

export default async (
    scene: Phaser.Scene,
    key: string,
    url: string,
    filetype: string): Promise<string> => {
    return new Promise((res, rej) => {
        if (assetExistence(key, filetype, scene)) {
            res(key)
            return
        }
        scene.load[filetype](key, url)
        const thisCompleteEvent = `filecomplete-${filetype}-${key}`
        scene.load.once(thisCompleteEvent, (_keyy: string, _path: string) => {
            res(key)
        })
        scene.load.once('loaderror', (file: { key: string }) => {
            if (file.key === key) {
                rej(file)
                scene.load.removeListener(thisCompleteEvent)
            }
        })
        scene.load.start()
    })
}

function assetExistence(key: string, filetype: string, scene: Phaser.Scene): boolean {
    let exists: boolean = false
    switch (filetype) {
        case "image": {
            exists = scene.textures.exists(key)
            break
        }
        case "json": {
            exists = scene.cache.json.exists(key)
            break
        }
        case "text": {
            exists = scene.cache.text.exists(key)
            break
        }
        case "sound": {
            exists = scene.cache.audio.exists(key)
            break
        }

    }
    return exists
}
