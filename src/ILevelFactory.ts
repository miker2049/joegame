/*
 * A factory for creating things a level needs
 */
interface ILevelFactory {
    createDepthMapper(): void
    createTilemap(): Phaser.Tilemaps.Tilemap
    createPlayer(): Phaser.Physics.Arcade.Sprite
    // createControls()
}
