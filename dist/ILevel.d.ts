import 'phaser';
import { ICharacter } from './ICharacter';
import { IMachineRegistry } from './components/MachineRegistry';
import { BondageResults } from 'bondage';
export interface IMapData {
    tileWidth: number;
    tileHeight: number;
    height: number;
    width: number;
    tilesets: Phaser.Tilemaps.Tileset[];
}
export interface IMapMethods {
    getObjectLayer(layer: string): Phaser.Tilemaps.ObjectLayer;
    getLayer(layer: string): Phaser.Tilemaps.LayerData;
    getTileAt(x: number, y: number, nonNull: boolean, layer: string): {
        index: number;
        properties?: {
            collides?: boolean;
        };
    };
}
export interface IMap extends IMapData, IMapMethods {
}
export interface DialogueRunner {
    getRunner(node: string): Generator<BondageResults, BondageResults, undefined>;
}
export interface ILevelComponents {
    map: Phaser.Tilemaps.Tilemap;
    player?: ICharacter;
    pathfinder: IPathfinder;
    npcs: Phaser.Physics.Arcade.Group;
    platforms: Phaser.Physics.Arcade.Group;
    scene: Phaser.Scene;
    machineRegistry: IMachineRegistry;
    key: string;
}
export interface IPathfinder {
    avoidAdditionalPoint(x: number, y: number): void;
    stopAvoidingAdditionalPoint(x: number, y: number): void;
    findPath(x: number, y: number, dx: number, dy: number, cb: (path: any) => void): number;
    calculate(): void;
}
