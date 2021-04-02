/**
 * @author       Richard Davey <rich@photonstorm.com>
 * @copyright    2021 Photon Storm Ltd.
 */
import Phaser from 'phaser';
export declare class WipePostFX extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
    /**
     * The progress of the wipe effect. From 0 to 1.
     *
     * @type {number}
     * @memberof WipePostFX
     */
    progress: number;
    /**
     * The width of the wipe effect.
     *
     * Given as a percentage of the overall texture width, between 0 and 1.
     *
     * @type {number}
     * @memberof WipePostFX
     * @default 0.1
     */
    wipeWidth: number;
    /**
     * The direction of the effect.
     *
     * @type {number}
     * @memberof WipePostFX
     * @private
     */
    direction: number;
    /**
     * The axis of the effect.
     *
     * @type {number}
     * @memberof WipePostFX
     * @private
     */
    axis: number;
    /**
     * Is this a reveal (1) or a wipe (0) ?
     *
     * @type {number}
     * @memberof WipePostFX
     * @private
     */
    reveal: number;
    /**
     * The WebGL Texture being 'wiped to'.
     *
     * @type {WebGLTexture}
     * @memberof WipePostFX
     */
    wipeTexture: WebGLTexture;
    /**
     * Creates an instance of WipePostFX.
     *
     * @param {Phaser.Game} game
     * @memberof WipePostFX
     */
    constructor(game: Phaser.Game);
    /**
     * @ignore
     */
    onBoot(): void;
    /**
     * Set the width of the wipe effect.
     *
     * The value is given as a percentage of the overall texture width, from 0 to 1.
     *
     * @param {number} [width=0.1] - The width of the effect.
     * @returns {this}
     * @memberof WipePostFX
     */
    setWipeWidth(width?: number): this;
    /**
     * Set the effect to use a Left to Right transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setLeftToRight(): this;
    /**
     * Set the effect to use a Right to Left transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setRightToLeft(): this;
    /**
     * Set the effect to use a Top to Bottom transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setTopToBottom(): this;
    /**
     * Set the effect to use a Bottom to Top transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setBottomToTop(): this;
    /**
     * Use a wipe effect.
     *
     * A wipe effect will wipe from one texture to another.
     *
     * The alternative is {@link setRevealEffect}.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setWipeEffect(): this;
    /**
     * Use a reveal effect.
     *
     * A reveal effect will wipe from a blank (invisible) texture to the object this pipeline is applied to.
     *
     * The alternative is {@link setWipeEffect}.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setRevealEffect(): this;
    /**
     * Set the texture to be wiped-to, or revealed.
     *
     * The texture must be already loaded and available from the Texture Manager.
     *
     * @param {string} [texture='__DEFAULT'] - The key of the texture to use.
     * @returns {this}
     * @memberof WipePostFX
     */
    setTexture(texture?: string): this;
    /**
     * Sets the progress of this effect.
     *
     * Progress is given as a value between 0 and 1.
     *
     * @param {number} [value=0] - The progress of the effect.
     * @returns {this}
     * @memberof WipePostFX
     */
    setProgress(value?: number): this;
    /**
     * @ignore
     */
    onPreRender(): void;
    /**
     * @ignore
     */
    onDraw(renderTarget: Phaser.Renderer.WebGL.RenderTarget): void;
}
