import { CharMoveAnims } from '../joegameTypes'
enum wikientries {
    character,
    spritesheet,
    image,
    platform,
    mapobject
}

export interface wikiCharacterEntry {
    name: string
    texture: string
    anims: CharMoveAnims
    charGroups: string[]
    speed?: number
    dashDistance?: number
    scale?: number
    body?: {
        offsetY?: number
        offsetX?: number
        width?: number
        height?: number
    }
}
export interface wikiImageEntry {
    key: string
    url: string
}
export interface wikiPlatformEntry {
    name: string
    texture: string
    groundTiles: number[]
    edgeTiles: number[]
}
export interface wikiMapobjectEntry {
    name: string
    req_spritesheet: string[]
    req_image: string[]
}
export interface wikiSpritesheetEntry {
    key: string
    url: string
    animLength?: number
    frameConfig: {
        frameWidth: number
        frameHeight: number
        margin: number
        spacing: number
    }
}
type wikiEntryTypes =
    | "spritesheet"
    | "image"
    | "character"
    | "platform"
    | "globals"
    | "mapobject"

export interface IWikiData {
    spritesheet: Map<string, wikiSpritesheetEntry>
    character: Map<string, wikiCharacterEntry>
    image: Map<string, wikiImageEntry>
    platform: Map<string, wikiPlatformEntry>
    mapobject: Map<string, wikiMapobjectEntry>
}


export function parsewikidata(rawwikidata): IWikiData {
    let tmpdata: IWikiData = {
        spritesheet: new Map<string, wikiSpritesheetEntry>(),
        character: new Map<string, wikiCharacterEntry>(),
        image: new Map<string, wikiImageEntry>(),
        platform: new Map<string, wikiPlatformEntry>(),
        mapobject: new Map<string, wikiMapobjectEntry>()
    }

    rawwikidata.forEach((page) => {
        page.forEach((item) => {
            if (item.type) {
                switch (item.type) {
                    case wikientries[wikientries.character]:
                        tmpdata.character.set(item.name, item)
                        break

                    case wikientries[wikientries.spritesheet]:
                        tmpdata.spritesheet.set(item.key, item)
                        break

                    case wikientries[wikientries.image]:
                        tmpdata.image.set(item.key || item.name, item)
                        break

                    case wikientries[wikientries.platform]:
                        tmpdata.platform.set(item.name, item)
                        break

                    case wikientries[wikientries.mapobject]:
                        tmpdata.mapobject.set(item.name, item)
                        break

                }
            }
        });
    })
    // console.log(JSON.stringify(tmpdata, function(key, value) {
    //     if (value instanceof Map) {
    //         return {
    //             dataType: 'Map',
    //             value: Array.from(value.entries()), // or with spread: value: [...value]
    //         };
    //     } else {
    //         return value;
    //     }
    // }))
    return tmpdata
}

