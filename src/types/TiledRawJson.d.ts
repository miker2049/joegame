type PropertyType = "string" | "int" | "float" | "bool" | "color" | "file" | "object" | "class"
interface Export {
    format: string;
    target: string;
}

interface Editorsettings {
    export: Export;
}

interface Property {
    name: string;
    type: PropertyType;
    value: any;
}

interface Object {
    charGroup: string;
    height: number;
    id: number;
    name: string;
    properties: Property[];
    rotation: number;
    type: string;
    visible: boolean;
    width: number;
    x: number;
    y: number;
    gid?: number;
    point?: boolean;
}

export interface ILayer {
    data: any[];
    height: number;
    id: number;
    name: string;
    opacity: number;
    properties: Property[];
    type: string;
    visible: boolean;
    width: number;
    x: number;
    y: number;
    draworder: string;
    objects: Object[];
}

interface TileAnimation {
    duration: number
    tileid: number
}

interface Tile {
    id: number;
    properties: Property[]
    image: string;
    imageheight: number;
    imagewidth: number;
    animation?: TileAnimation[]
    objectgroup?: TileObjectGroup
}

interface TileObjectGroup{
    id: number,
    objects: {
        height: number
        width: number
        x: number
        y: number
    }[]
}
interface Tileset {
    columns: number;
    firstgid: number;
    image: string;
    imageheight: number;
    imagewidth: number;
    margin: number;
    name: string;
    spacing: number;
    tilecount: number;
    tileheight: number;
    tiles: Tile[];
    tilewidth: number;
    source?: string
}

export default interface TiledRawJSON {
    compressionlevel: number;
    editorsettings: Editorsettings;
    height: number;
    infinite: boolean;
    layers: ILayer[];
    nextlayerid: number;
    nextobjectid: number;
    orientation: string;
    properties: Property[];
    renderorder: string;
    tiledversion: string;
    tileheight: number;
    tilesets: Tileset[];
    tilewidth: number;
    type: string;
    version: number;
    width: number;
}
