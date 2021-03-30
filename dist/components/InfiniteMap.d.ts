import 'phaser';
import { Dir } from '../joegameTypes';
import { IMap } from '../ILevel';
interface IInfiniteMapParams {
    scene: Phaser.Scene;
    tilemap: Phaser.Tilemaps.Tilemap;
    mapObjects: Phaser.GameObjects.Image[];
}
interface IInfiniteMap {
    shiftNorth(): void;
    shiftSouth(): void;
    shiftEast(): void;
    shiftWest(): void;
}
interface IMapObjectWatcher {
    watchNorth: number;
    watchSouth: number;
    watchEast: number;
    watchWest: number;
    obj: Phaser.GameObjects.Image;
    shift(dir: Dir): void;
    check(x: number, y: number): void;
}
export declare class MapObjectWatcher implements IMapObjectWatcher {
    obj: Phaser.GameObjects.Image;
    tileSize: number;
    mapTWidth: number;
    mapTHeight: number;
    limit: number;
    constructor(obj: Phaser.GameObjects.Image, limit: number, tileSize: number, mapTWidth: number, mapTHeight: number);
    shift(dir: Dir): void;
    get watchNorth(): number;
    get watchSouth(): number;
    get watchEast(): number;
    get watchWest(): number;
    check(x: number, y: number): void;
}
export declare class InfiniteMap implements IMap, IInfiniteMap {
    tilemap: Phaser.Tilemaps.Tilemap;
    cameraCenter: Phaser.Types.Math.Vector2Like;
    tileOffsetX: number;
    tileOffsetY: number;
    tileSize: number;
    mapTileWidth: number;
    mapTileHeight: number;
    objWatchers: IMapObjectWatcher[];
    constructor(params: IInfiniteMapParams);
    get tileWidth(): number;
    get tileHeight(): number;
    get height(): number;
    get width(): number;
    get tilesets(): Phaser.Tilemaps.Tileset[];
    getObjectLayer(layer: string): Phaser.Tilemaps.ObjectLayer;
    getLayer(layer: string): Phaser.Tilemaps.LayerData;
    getTileAt(x: number, y: number, nonNull: boolean, layer: string): {
        index: number;
        properties?: {
            collides?: boolean;
        };
    };
    shiftNorth(): void;
    shiftSouth(): void;
    shiftEast(): void;
    shiftWest(): void;
}
export {};
//# sourceMappingURL=InfiniteMap.d.ts.map