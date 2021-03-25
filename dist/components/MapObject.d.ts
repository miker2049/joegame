import 'phaser';
import { IMap } from '../ILevel';
export interface ITiledMapObject extends Phaser.Types.Tilemaps.TiledObject {
    depth: number;
}
export interface IMapObject {
    name: string;
    id: number;
    tiledWidth: number;
    tiledHeight: number;
    props: object;
    playAnim(): void;
    stopAnim(): void;
    x: number;
    y: number;
}
export declare class MapObject extends Phaser.GameObjects.Sprite implements IMapObject {
    name: string;
    id: number;
    tiledWidth: number;
    tiledHeight: number;
    props: object;
    constructor(scene: Phaser.Scene, tilemap: IMap, x: number, y: number, t_obj: ITiledMapObject);
    playAnim(): void;
    stopAnim(): void;
}
