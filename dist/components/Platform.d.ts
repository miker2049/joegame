import 'phaser';
import { ILevelComponents } from '../ILevel';
interface PlatformConfig {
    level: ILevelComponents;
    x: number;
    y: number;
    width: number;
    height: number;
    endX?: number;
    endY?: number;
    speed: number;
    name: string;
    ptype?: string;
}
export default class Platform extends Phaser.GameObjects.Container {
    speed: number;
    acceleration: number;
    pause: number;
    velX: number;
    velY: number;
    atHome: boolean;
    tileSize: number;
    body: Phaser.Physics.Arcade.Body;
    level: ILevelComponents;
    constructor(config: PlatformConfig);
    runPlatform(config: PlatformConfig): void;
    notifyVelChange(): void;
}
export {};
