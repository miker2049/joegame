import 'phaser';
export default abstract class {
    abstract createTilemap(): Phaser.Tilemaps.Tilemap;
    abstract createNPCs(): Phaser.Physics.Arcade.Group;
    abstract createPlayer(): Phaser.Physics.Arcade.Sprite;
}
