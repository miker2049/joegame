import { Application, Container } from "pixi.js";
import { TileCache } from "./utils";
import { Viewport } from "pixi-viewport";

export type TwoNums = [number, number];
export type Pnt = TwoNums;
export type MapAddress = [number, number, number, number];
export type SetCurrentMapFunction = (
    x: number,
    y: number,
    file: number,
    rank: number,
) => Promise<Container | undefined>;

export type DefaultParameters = {
    tileSize: number;
    tcache: TileCache;
    app: Application;
    viewport: Viewport;
    setCurrentMap: SetCurrentMapFunction;
    zoomLevel: number;
};

export type BaseLayer = {
    update: (x: number, y: number, z: number) => void;
} & Container;

type TileConfig = {
    tiles: number[];
    collision?: (0 | 1)[];
    texture: keyof JDB["images"];
    width: number;
};
export type MapObjectConfig = {
    tile_config: TileConfig;
    req_image: (keyof JDB["images"])[];
};

export type CharacterConfig = {
    name: string;
    texture: keyof JDB["images"];
    anims: Record<string, number[]>;
};

export type AssetConfig = {
    source: string;
    frameConfig?: {
        columns: number;
        tilecount: number;
        imagewidth: number;
        imageheight: number;
        spacing: number;
        margin: number;
        frameHeight: number;
        frameWidth: number;
    };
    animLength: number;
    url: string;
    key: string;
};

export type JDB = {
    mapobjects: Record<string, MapObjectConfig>;
    images: Record<string, AssetConfig>;
    characters: Record<string, CharacterConfig>;
};

export type ObjectRecord = [string, number, number];
export type WorldMapResponse = {
    wang: {
        data: string;
        name: string;
    }[];
    chars: string[];
    objects: ObjectRecord[];
};
