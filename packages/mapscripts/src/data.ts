import datajson from "assets/data.json";

type PlatformType = Record<
    string,
    {
        name: string;
        texture: string;
        groundTiles: number[];
        edgeTiles: number[];
    }
>;

type ImageType = Record<
    string,
    {
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
    }
>;

type SoundType = Record<
    string,
    {
        key: string;
        url: string;
        splitLength: number;
    }
>;

type MapObjectType = Record<
    string,
    {
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
    }
>;

type CharacterData = Record<
    string,
    {
        name: string;
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
    }
>;

type JoegameData = {
    platform: PlatformType;
    image: ImageType;
    sound: SoundType;
    mapobject: MapObjectType;
    character: CharacterData;
};

const data: JoegameData = datajson;
export async function getCharacter(name: string) {
    return data.character[name];
}
export async function getObject(name: string) {
    return data.mapobject[name];
}
export async function getImage(name: string) {
    return data.image[name];
}
