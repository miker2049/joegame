
interface Export {
    format: string;
    target: string;
}

interface Editorsettings {
    export: Export;
}

interface Property {
    name: string;
    type: string;
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

interface Layer {
    data: any[];
    height: number;
    id: number;
    name: string;
    opacity: number;
    type: string;
    visible: boolean;
    width: number;
    x: number;
    y: number;
    draworder: string;
    objects: Object[];
}

interface Property2 {
    name: string;
    type: string;
    value: string;
}

interface Property3 {
    name: string;
    type: string;
    value: boolean;
}

interface Tile {
    id: number;
    properties: Property3[];
    image: string;
    imageheight: number;
    imagewidth: number;
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
}

export default interface TiledRawJSON {
    compressionlevel: number;
    editorsettings: Editorsettings;
    height: number;
    infinite: boolean;
    layers: Layer[];
    nextlayerid: number;
    nextobjectid: number;
    orientation: string;
    properties: Property2[];
    renderorder: string;
    tiledversion: string;
    tileheight: number;
    tilesets: Tileset[];
    tilewidth: number;
    type: string;
    version: number;
    width: number;
}
