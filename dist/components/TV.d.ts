import 'phaser';
import BaseScene from './BaseScene';
import Level from './Level';
export default class TV extends Phaser.GameObjects.Sprite {
    video: Phaser.GameObjects.Video;
    scene: Level;
    constructor(scene: BaseScene, x: number, y: number);
    initColliders(): void;
    initVideo(vid: string): Phaser.GameObjects.Video;
    registerColliders(): void;
}
