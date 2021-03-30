import { Dir } from '../joegameTypes';
export interface ImoveDistanceObject {
    x: number;
    y: number;
    speed: number;
    move(dir: Dir): void;
    stop(face?: Dir): void;
    align(): Phaser.Types.Math.Vector2Like;
}
interface ImoveDistanceParams {
    gobject: ImoveDistanceObject;
    dir: Dir;
    distance: number;
    stop?: boolean;
}
export default function (params: ImoveDistanceParams): Promise<any>;
export {};
//# sourceMappingURL=moveDistance.d.ts.map