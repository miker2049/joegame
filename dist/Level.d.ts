import { ICharacter } from './ICharacter';
import { IMachineRegistry } from './components/MachineRegistry';
import { ILevelComponents, IPathfinder } from './ILevel';
export declare class Level implements ILevelComponents {
    map: Phaser.Tilemaps.Tilemap;
    player?: ICharacter;
    pathfinder: IPathfinder;
    npcs: Phaser.Physics.Arcade.Group;
    platforms: Phaser.Physics.Arcade.Group;
    scene: Phaser.Scene;
    machineRegistry: IMachineRegistry;
    key: string;
    constructor(game: Phaser.Game, mapjsonpath?: string);
}
