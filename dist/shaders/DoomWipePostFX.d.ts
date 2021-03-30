/**
 * @author       Richard Davey <rich@photonstorm.com>
 * @author       Zeh Fernando
 * @copyright    2021 Photon Storm Ltd.
 */
import Phaser from 'phaser';
export declare class DoomWipePostFX extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
    /**
     * The progress of the transition effect. From 0 to 1.
     *
     * @type {number}
     * @memberof DoomWipePostFX
     */
    progress: number;
    /**
     * The WebGL Texture being transitioned to.
     *
     * @type {WebGLTexture}
     * @memberof DoomWipePostFX
     */
    targetTexture: WebGLTexture;
    /**
     * The resize mode to be used for the target texture.
     *
     * Can be either 0, 1 or 2, for stretch, contain and cover modes respectively.
     *
     * The default is 'contain'.
     *
     * Set via the `setResizeMode` method.
     *
     * @type {number}
     * @memberof DoomWipePostFX
     */
    resizeMode: number;
    /**
     * The ratio of the target texture (width / height).
     *
     * This is set automatically in the `setTexture` method.
     *
     * @type {number}
     * @memberof DoomWipePostFX
     */
    toRatio: number;
    /**
     * The total number of bars / columns.
     *
     * @type {number}
     * @memberof DoomWipePostFX
     */
    bars: number;
    /**
     * Multiplier for the speed ratio.
     *
     * 0 = No variation when going down.
     * Higher = Some elements go much faster.
     *
     * @type {number}
     * @memberof DoomWipePostFX
     */
    amplitude: number;
    /**
     * Further variations in speed.
     *
     * 0 = No noise
     * 1 = Super noisy (ignore frequency)
     *
     * @type {number}
     * @memberof DoomWipePostFX
     */
    noise: number;
    /**
     * Horizontal speed variation.
     *
     * The bigger the value, the shorter the waves.
     *
     * @type {number}
     * @memberof DoomWipePostFX
     */
    frequency: number;
    /**
     * How much the bars seem to "run" from the middle of the screen
     * first (sticking to the sides).
     *
     * 0 = No drip
     * 1 = Curved drip
     *
     * @type {number}
     * @memberof DoomWipePostFX
     */
    dripScale: number;
    /**
     * The Doom Wipe Post FX is an effect that allows you to transition
     * between two objects via an effect that looks like the effect used
     * in the classic FPS game Doom.
     *
     * You can control the number of bars, amplitude, frequency and more.
     *
     * The source image comes from the Game Object to which the FX is applied,
     * which can be any Game Object that supports post pipelines, such as a
     * Sprite, Rope or Layer. You can also transition Cameras and even entire
     * Scenes. Please see the examples and class docs for further details.
     *
     * @param {Phaser.Game} game
     * @memberof DoomWipePostFX
     */
    constructor(game: Phaser.Game);
    /**
     * @ignore
     */
    onBoot(): void;
    /**
     * Set the resize mode of the target texture.
     *
     * Can be either:
     *
     * 0 - Stretch. The target texture is stretched to the size of the source texture.
     * 1 - Contain. The target texture is resized to fit the source texture. This is the default.
     * 2 - Cover. The target texture is resized to cover the source texture.
     *
     * If the source and target textures are the same size, then use a resize mode of zero
     * for speed.
     *
     * @param {number} [mode=1] - The Resize Mode. Either 0, 1 or 2.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setResizeMode(mode?: number): this;
    /**
     * Set the texture to be transitioned to.
     *
     * The texture must be already loaded and available from the Texture Manager.
     *
     * You can optionally also set the resize mode. This can be either:
     *
     * 0 - Stretch. The target texture is stretched to the size of the source texture.
     * 1 - Contain. The target texture is resized to fit the source texture. This is the default.
     * 2 - Cover. The target texture is resized to cover the source texture.
     *
     * If the source and target textures are the same size, then use a resize mode of zero
     * for speed.
     *
     * @param {string} [texture='__DEFAULT'] - The key of the texture to use.
     * @param {number} [mode] - The Resize Mode. Either 0, 1 or 2.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setTexture(texture?: string, resizeMode?: number): this;
    /**
     * Sets the progress of this effect.
     *
     * Progress is given as a value between 0 and 1.
     *
     * You can call this method at any point, or modify the `progress` property
     * directly for the same result. This can be done via tweens, Scene transitions,
     * Loader progress updates or any other system.
     *
     * @param {number} [value=0] - The progress of the effect. A value between 0 and 1.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setProgress(value?: number): this;
    /**
     * Sets the number of bars / columns that drop down.
     *
     * @param {number} [value=30] - The number of bars / columns.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setBars(value?: number): this;
    /**
     * Sets the multiplier for the drop speed ratio.
     *
     * 0 = No variation when going down.
     * Higher = Some elements go much faster.
     *
     * @param {number} [value=2] - The amplitude.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setAmplitude(value?: number): this;
    /**
     * Further variations in speed.
     *
     * 0 = No noise
     * 1 = Super noisy (ignore frequency)
     *
     * @param {number} [value=0.1] - The noise.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setNoise(value?: number): this;
    /**
     * Horizontal speed variation.
     *
     * The bigger the value, the shorter the waves.
     *
     * @param {number} [value=0.5] - The frequency.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setFrequency(value?: number): this;
    /**
     * How much the bars seem to "run" from the middle of the screen
     * first (sticking to the sides).
     *
     * 0 = No drip
     * 1 = Curved drip
     *
     * @param {number} [value=0.5] - The drip scale.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setDripScale(value?: number): this;
    /**
     * @ignore
     */
    onPreRender(): void;
    /**
     * @ignore
     */
    onDraw(renderTarget: Phaser.Renderer.WebGL.RenderTarget): void;
}
//# sourceMappingURL=DoomWipePostFX.d.ts.map