import Phaser from 'phaser';
export default class Blobs extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
    /**
     * The Water Drop Post FX is an effect that allows you to transition
     * between two objects via an effect that looks like water rippling
     * out from the surface. You can control the amplitude and speed of
     * the ripple.
     *
     * The source image comes from the Game Object to which the FX is applied,
     * which can be any Game Object that supports post pipelines, such as a
     * Sprite, Rope or Layer. You can also transition Cameras and even entire
     * Scenes. Please see the examples and class docs for further details.
     *
     * @param {Phaser.Game} game
     * @memberof WaterDropPostFX
     */
    constructor(game: Phaser.Game);
    /**
     * @ignore
     */
    onBoot(): void;
    onPreRender(): void;
    onDraw(renderTarget: Phaser.Renderer.WebGL.RenderTarget): void;
}
