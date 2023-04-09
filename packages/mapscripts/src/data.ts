// -*- lsp-enabled-clients: (deno-ls); -*-
import datajson from "../../../assets/data.json" assert { type: "json" };
type PlatformData = {
    name: string;
    texture: string;
    groundTiles: number[];
    edgeTiles: number[];
};
type Platforms = Record<string, PlatformData>;
type ImageData = {
    key: string;
    url?: string;
    source: string;
    frameConfig?: {
        frameWidth: number;
        frameHeight: number;
        imageheight: number;
        imagewidth: number;
        tilecount: number;
        columns: number;
        margin?: number;
        spacing?: number;
    };
};
type Images = Record<string, ImageData>;

type SoundType = Record<
    string,
    {
        key: string;
        url: string;
        splitLength: number;
    }
>;

export type MapObjectData = {
    name?: string;
    texture?: string;
    req_image: string[];
    width?: number;
    height?: number;
    tile_config?: {
        width?: number;
        texture: string;
        tiles: number[];
        pick?: boolean;
    };
    body_config?: {
        x: number;
        y: number;
        width: number;
        height: number;
    };
};
type MapObjects = Record<string, MapObjectData>;

type CharacterData = {
    name: string;
    inherits?: string[];
    texture: string;
    width?: number;
    height?: number;
    speed?: number;
    scale?: number;
    anims: Record<string, number[]>;
    body?: {
        offsetX?: number;
        offsetY?: number;
        width?: number;
        height?: number;
    };
};

type Characters = Record<string, CharacterData>;
type JoegameData = {
    platform: Platforms;
    image: Images;
    sound: SoundType;
    mapobject: MapObjects;
    character: Characters;
};

//ts ignore because we know the data is valid
// @ts-ignore
const data: JoegameData = datajson;

export async function getCharacter(
    name: keyof Characters
): Promise<CharacterData | undefined> {
    const character = data.character[name];
    if (!character || !character.inherits) return character;
    else if (character.inherits.length > 0) {
        const inheritedprops = await Promise.all(
            character.inherits.map((c) => getCharacter(c))
        );
        const mergedprops = inheritedprops.reduce((acc, props) => {
            return { ...acc, ...props };
        }, {});
        return { ...mergedprops, ...character };
    }
    return character;
}

export async function getObject(
    name: keyof MapObjects
): Promise<MapObjectData | undefined> {
    return data.mapobject[name];
}

export async function getImage(name: string): Promise<ImageData | undefined> {
    return data.image[name];
}
