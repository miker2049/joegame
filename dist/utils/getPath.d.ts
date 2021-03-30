import { IPathfinder } from '../ILevel';
export interface getPathParams {
    x: number;
    y: number;
    dx: number;
    dy: number;
    tempObsX?: number;
    tempObsY?: number;
    finder: IPathfinder;
}
export default function (params: getPathParams): Promise<Phaser.Types.Math.Vector2Like[]>;
//# sourceMappingURL=getPath.d.ts.map