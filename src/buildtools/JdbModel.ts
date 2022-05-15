enum AssetType {
    image = 1, sound = 2,
    text = 3, shader = 4,
    wasm = 5, midifile = 6,
    video = 7, gif = 8
}

export class JdbTable {
    id: number | undefined = undefined
    getCols():  string[] {
        return Object.keys(this)
    }
}

export class JdbAsset extends JdbTable {
    filename: string = "default-asset-name"
    hash: string = "0"
    creator: string = "mike"
    asset_source: string = "joegame.org"
    asset_type: AssetType = 1
    blob_data: Buffer =  Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAgAAAAICAAAAADhZOFXAAAAAXNSR0IArs4c6QAAACtJREFUCJk9y8ENADAIw8CDATMsE/ZB27ws2anYNWYWhsFVab8JRBNCvfsB3wUFRZjZdD0AAAAASUVORK5CYII=', "base64")
}

export class JdbImage extends JdbTable {
    asset: number = 0
    key_name: string = "default-image-name"
    anim_length: number = -1
    frame_width: number = 16
    frame_height: number = 16
    margin: number = 0
    spacing: number = 0
}

export class JdbMapObject extends JdbTable {
    name: string = "default-map-object"
    width: number = 16
    height: number = 16
    texture: string = "default-texture"
    frame: number = 0
    flip_x: boolean = false
    flip_y: boolean = false
    depth: number = 1
    rotation: number = 0
    origin_x: number = 0
    origin_y: number =  0
    scrollFactor: number = 1
    visible: boolean = true
    body_x: number = 0
    body_y: number = 0
    body_width: number = 16
    body_height: number = 16
    moveable: boolean = false
    tint: number = 0
    tile_layer: string = "mapobject-tilelayer"
    tile_alterations: number[] = []
    popupText: string = "popup text"
}

export class JdbBody extends JdbTable {
    anim_north: number = 0
    anim_south: number = 0
    anim_east: number = 0
    anim_west: number = 0
    speed: number = 16
    dash_distance: number = 16
    scale: number = 1
    body_offset_x: number = 0
    body_offset_y: number = 0
    width: number = 16
    height: number = 16
}

export class JdbCharacter extends JdbTable {
    character_name: string = "default-char"
    body: number = 0
}

export enum JdbTableNames {
    assets = 'assets',images = 'images',
    mapobjects = 'mapobjects', bodies = 'bodies',
    characters = 'characters'
}
export class JdbModels {
    schema: Map<JdbTableNames, JdbTable>
    constructor(){
        this.schema = new Map<JdbTableNames, JdbTable>()
        this.schema.set(JdbTableNames.assets, new JdbAsset())
        this.schema.set(JdbTableNames.images, new JdbImage())
        this.schema.set(JdbTableNames.mapobjects, new JdbMapObject())
        this.schema.set(JdbTableNames.bodies, new JdbBody())
        this.schema.set(JdbTableNames.characters, new JdbCharacter())
    }
}
// export interface JDB {
//     Character: Character
//     Body: Body
// }
