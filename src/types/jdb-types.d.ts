enum AssetType {
    image = 1, sound = 2,
    text = 3, shader = 4,
    wasm = 5, midifile = 6,
    video = 7, gif = 8
}
export interface Asset {
    id: number
    name: string
    hash: string
    creator: string
    asset_source: string
    asset_type: AssetType
    blob_data: Buffer
}

export interface Image {
    id: number
    asset: number
    key_name: string
    anim_length: number
    frame_width: number
    frame_height: number
    margin: number
    spacing: number
}

export interface MapObject {
    id: number
    name: string
    width: number
    height: number
    texture: number
    frame: number
    flip_x: number
    flip_y: number
    depth: number
    rotation: number
    origin_x: number
    origin_y: number
    scrollFactor: number
    visible: number
    body_x: number
    body_y: number
    body_width: number
    body_height: number
    moveable: number
    tint: number
    tile_layer: string
    tile_alterations: number[]
    popupText: string
}

export interface Body {
    id: number
    anim_north: number
    anim_south: number
    anim_east: number
    anim_west: number
    speed: number
    dash_distance: number
    scale: number
    body_offset_x: number
    body_offset_y: number
    width: number
    height: number
}

export interface Character {
    id: number
    character_name: string
    body: number
}

// export interface JDB {
//     Character: Character
//     Body: Body
// }
