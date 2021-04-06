import TiledRawJSON from "../types/TiledRawJson"
import { getMapKeyNameRaw, getMapKeyName, getDialogueKeyName } from "./getKeyNames"
import { IWikiData, wikiCharacterEntry } from "./parseWikiData"
import wikiData from './wikiData'
import * as url from 'url'

/*
 * For loading assets from a LOADED raw tiled json
 * Both the tiled json and wikidata need to be available first
 */
export default function loadAssets(game: Phaser.Game, mapjsonpath: string): Promise<Phaser.Game> {
    return new Promise<Phaser.Game>((res, reject) => {
        const mapjson: TiledRawJSON = game.cache.json.get(getMapKeyNameRaw(mapjsonpath))
        const wikidata: IWikiData = wikiData(game)
        if (!mapjson || !wikidata) reject("wikidata and mapjson is not already loaded!")
        const scene = game.scene.getScenes(true)[0]
        // scene.load.setBaseURL(ASSETPATH)
        loadTilesets(scene, mapjson, mapjsonpath)
        loadObjectAssets(scene, mapjson, wikidata)
        scene.load.tilemapTiledJSON(getMapKeyName(mapjsonpath), mapjson)
        loadDialogueFile(scene, mapjsonpath)
        scene.load.once('complete', () => { res(game) })
        scene.load.once('loaderror', (file: Phaser.Loader.File) => {
            if (file.key != getDialogueKeyName(mapjsonpath)) {
                reject(file)
            } else {
                // console.log("loading default dialogue file")
                // scene.load.json(getDialogueKeyName(mapjsonpath), 'assets/dialogues/default_dialogue.json')
            }
        })

        scene.load.start()
        // res(game)
    })
}

function loadTilesets(scene: Phaser.Scene, mapjson: TiledRawJSON, path: string): void {
    mapjson.tilesets.forEach((t) => {
        if (t.image) {
            const fixpath = url.resolve(path, t.image)
            scene.load.spritesheet({ key: t.name, url: fixpath, frameConfig: { frameWidth: t.tilewidth, frameHeight: t.tileheight, margin: t.margin, spacing: t.spacing } })
        } else if (t.tiles) {
            //it is an "image collection" tileset, store it by firstgid and all that
            t.tiles.forEach((tiles) => {
                if (tiles.image) {
                    const fixpath = url.resolve(path, tiles.image)
                    scene.load.image((t.firstgid + tiles.id).toString(), fixpath)
                }
            });
        }
    });
}

function loadDialogueFile(scene: Phaser.Scene, mapjsonpath: string): void {
    scene.load.json(getDialogueKeyName(mapjsonpath), "assets/dialogues/" + getDialogueKeyName(mapjsonpath) + '.json')
}

function loadObjectAssets(scene: Phaser.Scene, mapjson: TiledRawJSON, wikidata: IWikiData): void {

    let characters: string[] = []
    let charGroups: string[] = []
    let mapobjects: string[] = []
    let platforms: string[] = []
    //TODO absolte path
    mapjson.layers.forEach((l) => {
        //getting the name of characters prestent on the map
        if (l.type === "objectgroup") {

            if (l.name === 'NPCs') {
                l.objects?.forEach((n) => {
                    if (n.name) {
                        characters.push(n.name)
                    } else {
                        charGroups.push(n.properties?.find(prop => prop.name === 'charGroup')?.value ?? 'all')
                        console.log(charGroups)
                    }
                });
            } else if (l.name === 'Platforms') {

                l.objects?.forEach((n) => {
                    platforms.push(n.type)
                })
            } else if (l.name === 'TweetConvos') {

                l.objects?.forEach((n) => {
                    charGroups.push(n.properties?.find(prop => prop.name === 'charGroup')?.value ?? 'all')
                })
            } else if (l.name === 'Player') {

                l.objects?.forEach((n) => {
                    charGroups.push(n.properties?.find(prop => prop.name === 'charGroup')?.value ?? 'all')
                })
            } else {

                l.objects?.forEach((n) => {
                    mapobjects.push(n.type)
                })
                //we are in object group, but not NPCs, so there might be mapobjects here
            }
        }
    });

    characters = Array.from(new Set(characters))
    charGroups = Array.from(new Set(charGroups))
    mapobjects = Array.from(new Set(mapobjects))
    platforms = Array.from(new Set(platforms))
    let spritesheets: string[] = [];
    let images: string[] = [];
    characters.forEach((n) => {
        const found = wikidata.character.get(n)
        if (found != undefined) {
            spritesheets.push(found.texture)
        }
    })
    charGroups.forEach((n) => {
        const group = Array.from(wikidata.character).filter(item => item[1].charGroups?.includes(n) || n === 'all');
        if (group != undefined) {
            group.forEach(e => spritesheets.push(e[1].texture))
            // spritesheets.push(found.texture)
        }
    })
    mapobjects.forEach((mo) => {
        const found = wikidata.mapobject.get(mo)
        if (found != undefined) {
            //TODO need req_otherthings too
            found.req_spritesheet.forEach((sheet) => spritesheets.push(sheet))
            found.req_image.forEach((image) => images.push(image))
        }
    })
    platforms.forEach((p) => {
        const found = wikidata.platform.get(p)
        if (found != undefined) {
            spritesheets.push(found.texture)
        }
    })

    spritesheets = Array.from(new Set(spritesheets))
    images = Array.from(new Set(images))

    spritesheets.forEach((s) => {
        const found = wikidata.spritesheet.get(s)
        if (found != undefined) {
            // found.url = url.resolve(TILEMAPDIR, found.url)
            scene.load.spritesheet(found)
        }
    });
    images.forEach((i) => {
        const found = wikidata.image.get(i)
        if (found != undefined) {
            // found.url = url.resolve(TILEMAPDIR, found.url)
            scene.load.image(found)
            // scene.textures.addBase64(found.key, await import(found.url))
        }
    });

}
