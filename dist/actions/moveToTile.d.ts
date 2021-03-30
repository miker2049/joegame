import 'phaser';
import { Dir } from '../joegameTypes';
import { IPathfinder } from '../ILevel';
interface ICharPathMoveControl {
    face(dir: Dir): void;
    move(dir: Dir): void;
    stop(): void;
    align(): void;
    charBody: Phaser.Physics.Arcade.Body;
}
export interface moveToTileParams {
    x: number;
    y: number;
    dx: number;
    dy: number;
    tempObsX?: number;
    tempObsY?: number;
    tileWidth: number;
    tileHeight: number;
    finalFacing?: Dir;
    charController: ICharPathMoveControl;
    finder: IPathfinder;
    tweener: Phaser.Tweens.TweenManager;
    speed: number;
    cb: (s: string) => void;
}
export declare function moveToTile(params: moveToTileParams): Phaser.Tweens.Timeline | string;
export {};
//# sourceMappingURL=moveToTile.d.ts.map