interface ILevelFactory {
    createDepthMapper(): void;
    createTilemap(): Phaser.Tilemaps.Tilemap;
    createPlayer(): Phaser.Physics.Arcade.Sprite;
}
