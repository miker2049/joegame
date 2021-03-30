/**
 * @author       Richard Davey <rich@photonstorm.com>
 * @author       laserdog
 * @copyright    2021 Photon Storm Ltd.
 */
import Phaser from 'phaser';
export declare class PageCurlPostFX extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
    /**
     * The WebGL Texture being transitioned to.
     *
     * @type {WebGLTexture}
     * @memberof PageCurlPostFX
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
     * @memberof PageCurlPostFX
     */
    resizeMode: number;
    /**
     * The ratio of the target texture (width / height).
     *
     * This is set automatically in the `setTexture` method.
     *
     * @type {number}
     * @memberof PageCurlPostFX
     */
    toRatio: number;
    /**
     * The radius of the curl.
     *
     * Use very low values. The default is 0.1.
     *
     * @type {number}
     * @memberof PageCurlPostFX
     */
    radius: number;
    /**
     * The position the effect runs from.
     *
     * See also the `setFrom` method.
     *
     * @type {Phaser.Math.Vector2}
     * @memberof PageCurlPostFX
     */
    from: Phaser.Math.Vector2;
    /**
     * The position the effect runs to.
     *
     * See also the `setTo` method.
     *
     * @type {Phaser.Math.Vector2}
     * @memberof PageCurlPostFX
     */
    to: Phaser.Math.Vector2;
    /**
     * The Page Curl FX allows you to create a classic 'page curl' motion
     * between two sources. The curl proceeds from either the top-left or
     * top-right of the source. You can control the position vectors
     * independantly, as well as the radius of the curl.
     *
     * The source image comes from the Game Object to which the FX is applied,
     * which can be any Game Object that supports post pipelines, such as a
     * Sprite, Rope or Layer. You can also transition Cameras and even entire
     * Scenes. Please see the examples and class docs for further details.
     *
     * @param {Phaser.Game} game
     * @memberof PageCurlPostFX
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
     * @memberof PageCurlPostFX
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
     * @memberof PageCurlPostFX
     */
    setTexture(texture?: string, resizeMode?: number): this;
    /**
     * Sets the position the effect should start from.
     *
     * The values are given in pixels and should typically be restricted
     * to the range of the texture size the effect is running on.
     *
     * @param {number} x - The x position.
     * @param {number} y - The y position.
     * @returns {this}
     * @memberof PageCurlPostFX
     */
    setFrom(x: number, y: number): this;
    /**
     * Sets the position the effect should run to.
     *
     * The values are given in pixels and should typically be restricted
     * to the range of the texture size the effect is running on.
     *
     * @param {number} x - The x position.
     * @param {number} y - The y position.
     * @returns {this}
     * @memberof PageCurlPostFX
     */
    setTo(x: number, y: number): this;
    /**
     * Sets the radius of the curl effect.
     *
     * Should be a small value. The defaul is 0.1.
     *
     * @param {number} [value=0.1] - The radius.
     * @returns {this}
     * @memberof PageCurlPostFX
    */
    setRadius(value?: number): this;
    /**
     * @ignore
     */
    onPreRender(): void;
    /**
     * @ignore
     */
    onDraw(renderTarget: Phaser.Renderer.WebGL.RenderTarget): void;
}
//# sourceMappingURL=PageCurlPostFX.d.ts.map