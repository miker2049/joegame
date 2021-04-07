import { CharMoveAnims } from '../joegameTypes';
export interface wikiCharacterEntry {
    name: string;
    texture: string;
    anims: CharMoveAnims;
    charGroups: string[];
    speed?: number;
    dashDistance?: number;
    scale?: number;
    body?: {
        offsetY?: number;
        offsetX?: number;
        width?: number;
        height?: number;
    };
}
export interface wikiImageEntry {
    key: string;
    url: string;
}
export interface wikiPlatformEntry {
    name: string;
    texture: string;
    groundTiles: number[];
    edgeTiles: number[];
}
export interface wikiMapobjectEntry {
    name: string;
    req_spritesheet: string[];
    req_image: string[];
}
export interface wikiSpritesheetEntry {
    key: string;
    url: string;
    animLength?: number;
    frameConfig: {
        frameWidth: number;
        frameHeight: number;
        margin?: number;
        spacing?: number;
    };
}
export interface IWikiData {
    spritesheet: Map<string, wikiSpritesheetEntry>;
    character: Map<string, wikiCharacterEntry>;
    image: Map<string, wikiImageEntry>;
    platform: Map<string, wikiPlatformEntry>;
    mapobject: Map<string, wikiMapobjectEntry>;
    convoManifest: string;
}
export declare function parseCSVRowsToWikiData(raw: string): IWikiData;
export declare function parsewikidata(rawwikidata: any): IWikiData;
