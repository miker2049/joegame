/**
 * @author       Richard Davey <rich@photonstorm.com>
 * @copyright    2021 Photon Storm Ltd.
 */

import Phaser from 'phaser'

const wipeFragShader = `
#define SHADER_NAME WIPE_FS

precision mediump float;

uniform sampler2D uMainSampler;
uniform sampler2D uMainSampler2;
uniform vec2 uResolution;
uniform vec4 uInput;
uniform float uReveal;

void main ()
{
    vec2 uv = gl_FragCoord.xy / uResolution.xy;

    vec4 color0;
    vec4 color1;
            
    if (uReveal == 0.0)
    {
        color0 = texture2D(uMainSampler, uv);
        color1 = texture2D(uMainSampler2, vec2(uv.x, 1.0 - uv.y));
    }
    else
    {
        color0 = texture2D(uMainSampler2, vec2(uv.x, 1.0 - uv.y));
        color1 = texture2D(uMainSampler, uv);
    }

    float distance = uInput.x;
    float width = uInput.y;
    float direction = uInput.z;
    float axis = uv.x;

    if (uInput.w == 1.0)
    {
        axis = uv.y;
    }

    float adjust = mix(width, -width, distance);
    
    float value = smoothstep(distance - width, distance + width, abs(direction - axis) + adjust);

    gl_FragColor = mix(color1, color0, value);
}
`;

export class WipePostFX extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
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
    constructor(game: Phaser.Game) {
        super({
            game,
            name: 'WipePostFX',
            shaders: [
                {
                    name: 'Wipe',
                    fragShader: wipeFragShader
                }
            ]
        });
        this.wipeTexture = {}
        this.progress = 0;
        this.wipeWidth = 0.1;
        this.direction = 0;
        this.axis = 0;
        this.reveal = 0;
    }

    /**
     * @ignore
     */
    onBoot(): void {
        this.setTexture();
    }

    /**
     * Set the width of the wipe effect.
     *
     * The value is given as a percentage of the overall texture width, from 0 to 1.
     *
     * @param {number} [width=0.1] - The width of the effect.
     * @returns {this}
     * @memberof WipePostFX
     */
    setWipeWidth(width: number = 0.1): this {
        this.wipeWidth = width;

        return this;
    }

    /**
     * Set the effect to use a Left to Right transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setLeftToRight(): this {
        this.direction = 0;
        this.axis = 0;

        return this;
    }

    /**
     * Set the effect to use a Right to Left transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setRightToLeft(): this {
        this.direction = 1;
        this.axis = 0;

        return this;
    }

    /**
     * Set the effect to use a Top to Bottom transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setTopToBottom(): this {
        this.direction = 1;
        this.axis = 1;

        return this;
    }

    /**
     * Set the effect to use a Bottom to Top transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */
    setBottomToTop(): this {
        this.direction = 0;
        this.axis = 1;

        return this;
    }

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
    setWipeEffect(): this {
        this.reveal = 0;

        return this;
    }

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
    setRevealEffect(): this {
        this.wipeTexture = this.game.textures.getFrame('__DEFAULT').glTexture;

        this.reveal = 1;

        return this;
    }

    /**
     * Set the texture to be wiped-to, or revealed.
     *
     * The texture must be already loaded and available from the Texture Manager.
     *
     * @param {string} [texture='__DEFAULT'] - The key of the texture to use.
     * @returns {this}
     * @memberof WipePostFX
     */
    setTexture(texture: string = '__DEFAULT'): this {
        const phaserTexture = this.game.textures.getFrame(texture);

        if (phaserTexture) {
            this.wipeTexture = phaserTexture.glTexture;
        }
        else {
            this.wipeTexture = this.game.textures.getFrame('__DEFAULT').glTexture;
        }

        this.set1i('uMainSampler2', 1);

        return this;
    }

    /**
     * Sets the progress of this effect.
     *
     * Progress is given as a value between 0 and 1.
     *
     * @param {number} [value=0] - The progress of the effect.
     * @returns {this}
     * @memberof WipePostFX
     */
    setProgress(value: number = 0): this {
        this.progress = value;

        return this;
    }

    /**
     * @ignore
     */
    onPreRender(): void {
        this.set4f('uInput', this.progress, this.wipeWidth, this.direction, this.axis);
        this.set1f('uReveal', this.reveal);
    }

    /**
     * @ignore
     */
    onDraw(renderTarget: Phaser.Renderer.WebGL.RenderTarget): void {
        this.set2f('uResolution', renderTarget.width, renderTarget.height);

        this.bindTexture(this.wipeTexture, 1);

        this.bindAndDraw(renderTarget);
    }
}
