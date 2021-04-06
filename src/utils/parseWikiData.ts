import { CharMoveAnims } from '../joegameTypes'
import { parse as csvParse } from 'papaparse'
import { characterCSVRow, gamedataCSVRow, imageCSVRow, mapobjectCSVRow, platformCSVRow, spritesheetCSVRow, convoManifestCSVRow } from './gameDataCSVTypes'

enum wikientries {
    character,
    spritesheet,
    image,
    platform,
    mapobject,
    convoManifest
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
        margin?: number
        spacing?: number
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
    convoManifest: string
}

const createTmpData = (): IWikiData => {
    return {
        spritesheet: new Map<string, wikiSpritesheetEntry>(),
        character: new Map<string, wikiCharacterEntry>(),
        image: new Map<string, wikiImageEntry>(),
        platform: new Map<string, wikiPlatformEntry>(),
        mapobject: new Map<string, wikiMapobjectEntry>(),
        convoManifest: ''
    }
}
export function parseCSVRowsToWikiData(raw: string): IWikiData {
    let parsed = csvParse<gamedataCSVRow>(raw, { dynamicTyping: true })
    let tmpdata = createTmpData()
    parsed.data.forEach((row) => {
        if (row[0] !== -1) {
            switch (row[1]) {
                case 'spritesheet': {
                    row = row as spritesheetCSVRow
                    tmpdata.spritesheet.set(row[2], {
                        key: row[2],
                        url: row[3],
                        animLength: row[4] != null ? row[4] as number : undefined,
                        frameConfig: {
                            frameWidth: row[5],
                            frameHeight: row[6],
                            margin: row[7],
                            spacing: row[8],
                        }
                    })
                    break
                }
                case 'image': {
                    row = row as imageCSVRow
                    tmpdata.image.set(row[2], {
                        key: row[2],
                        url: row[3],
                    })
                    break
                }
                case 'character': {
                    row = row as characterCSVRow
                    tmpdata.character.set(row[2], {
                        name: row[2],
                        texture: row[3],
                        anims: {
                            north: row[4],
                            south: row[5],
                            east: row[6],
                            west: row[7],
                        },
                        speed: row[8],
                        dashDistance: row[9],
                        scale: row[10],
                        body: {
                            offsetX: row[11],
                            offsetY: row[12],
                            width: row[13],
                            height: row[14]
                        },
                        charGroups: [row[15], row[16]]
                    })
                    break
                }
                case 'platform': {
                    row = row as platformCSVRow
                    tmpdata.platform.set(row[2], {
                        name: row[2],
                        texture: row[3],
                        groundTiles: `${row[4]}`.split(';').map(i => Number.parseInt(i)),
                        edgeTiles: `${row[5]}`.split(';').map(i => Number.parseInt(i))
                    })
                    break
                }
                case 'mapobject': {
                    row = row as mapobjectCSVRow
                    tmpdata.mapobject.set(row[2], {
                        name: row[2],
                        req_spritesheet: `${row[3]}`.split(';'),
                        req_image: `${row[4]}`.split(';')
                    })
                    break
                }
                case 'convoManifest': {
                    row = row as convoManifestCSVRow
                    tmpdata.convoManifest = row[2]
                    break
                }
            }
        }
    })
    return tmpdata
}

export function parsewikidata(rawwikidata): IWikiData {
    let tmpdata: IWikiData = createTmpData()
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
    return tmpdata
}

