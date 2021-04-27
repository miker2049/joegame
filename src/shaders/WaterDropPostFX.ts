/**
 * @author       Richard Davey <rich@photonstorm.com>
 * @author       Paweł Płóciennik
 * @copyright    2021 Photon Storm Ltd.
 */

import Phaser from 'phaser';

const fragShader = `
#define SHADER_NAME WATERDROP_FS

precision mediump float;

uniform sampler2D uMainSampler;
uniform sampler2D uMainSampler2;

uniform int resizeMode;
uniform float progress;
uniform float fromRatio;
uniform float toRatio;

varying vec2 outFragCoord;

//  Transition specific
uniform float amplitude;
uniform float speed;

vec4 getFromColor (vec2 uv)
{
    return texture2D(uMainSampler, uv);
}

vec4 getToColor (vec2 uv)
{
    if (resizeMode == 2)
    {
        //  cover
        return texture2D(uMainSampler2, 0.5 + (vec2(uv.x, 1.0 - uv.y) - 0.5) * vec2(min(fromRatio / toRatio, 1.0), min((toRatio / fromRatio), 1.0)));
    }
    else if (resizeMode == 1)
    {
        //  contain
        return texture2D(uMainSampler2, 0.5 + (vec2(uv.x, 1.0 - uv.y) - 0.5) * vec2(max(fromRatio / toRatio, 1.0), max((toRatio / fromRatio), 1.0)));
    }
    else
    {
        //  stretch
        return texture2D(uMainSampler2, vec2(uv.x, 1.0 - uv.y));
    }
}

// Transition Author: Paweł Płóciennik
// Transition License: MIT

vec4 transition (vec2 p)
{
    vec2 dir = p - vec2(0.5);

    float dist = length(dir);

    if (dist > progress)
    {
        return mix(getFromColor(p), getToColor(p), progress);
    }
    else
    {
        vec2 offset = dir * sin(dist * amplitude - progress * speed);

        return mix(getFromColor(p + offset), getToColor(p), progress);
    }
}
    
void main ()
{
    vec2 uv = outFragCoord;

    gl_FragColor = transition(uv);
}
`;

export class WaterDropPostFX extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
    /**
     * The progress of the transition effect. From 0 to 1.
     *
     * @type {number}
     * @memberof WaterDropPostFX
     */
    progress: number;

    /**
     * The WebGL Texture being transitioned to.
     *
     * @type {WebGLTexture}
     * @memberof WaterDropPostFX
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
     * @memberof WaterDropPostFX
     */
    resizeMode: number;

    /**
     * The ratio of the target texture (width / height).
     *
     * This is set automatically in the `setTexture` method.
     *
     * @type {number}
     * @memberof WaterDropPostFX
     */
    toRatio: number;

    /**
     * The amplitude of the effect.
     *
     * This controls how many 'ripples' there are.
     *
     * @type {number}
     * @memberof WaterDropPostFX
     */
    amplitude: number;

    /**
     * The speed of the effect.
     *
     * This controls how fast the ripples spread from the center.
     *
     * @type {number}
     * @memberof WaterDropPostFX
     */
    speed: number;

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
    constructor(game: Phaser.Game) {
        super({
            game,
            name: 'WaterDropPostFX',
            fragShader
        });

        this.progress = 0;
        this.resizeMode = 1;
        this.toRatio = 0;

        this.amplitude = 30;
        this.speed = 30;
        this.targetTexture = {}
    }

    /**
     * @ignore
     */
    onBoot(): void {
        this.setTexture();
    }

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
     * @memberof WaterDropPostFX
     */
    setResizeMode(mode: number = 1): this {
        this.resizeMode = mode;

        return this;
    }

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
     * @memberof WaterDropPostFX
     */
    setTexture(texture: string = '__DEFAULT', resizeMode?: number): this {
        let phaserTexture = this.game.textures.getFrame(texture);

        if (!phaserTexture) {
            phaserTexture = this.game.textures.getFrame('__DEFAULT');
        }

        this.toRatio = phaserTexture.width / phaserTexture.height;

        this.targetTexture = phaserTexture.glTexture;

        if (resizeMode !== undefined) {
            this.resizeMode = resizeMode;
        }

        this.set1i('uMainSampler2', 1);
        this.set1f('toRatio', this.toRatio);

        return this;
    }

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
     * @memberof WaterDropPostFX
     */
    setProgress(value: number = 0): this {
        this.progress = Phaser.Math.Clamp(value, 0, 1);

        return this;
    }

    /**
     * Sets the amplitude of the Water Drop effect.
     *
     * This controls the number of ripples.
     *
     * @param {number} [value=30] - The amplitude.
     * @returns {this}
     * @memberof WaterDropPostFX
     */
    setAmplitude(value: number = 30): this {
        this.amplitude = value;

        return this;
    }

    /**
     * Sets the speed of the Water Drop effect.
     *
     * This controls how fast the ripples spread from the center.
     *
     * @param {number} [value=30] - The amplitude.
     * @returns {this}
     * @memberof WaterDropPostFX
     */
    setSpeed(value: number = 30): this {
        this.speed = value;

        return this;
    }

    /**
     * @ignore
     */
    onPreRender(): void {
        this.set1f('progress', this.progress);
        this.set1i('resizeMode', this.resizeMode);
        this.set1f('amplitude', this.amplitude);
        this.set1f('speed', this.speed);
    }

    /**
     * @ignore
     */
    onDraw(renderTarget: Phaser.Renderer.WebGL.RenderTarget): void {
        this.set1f('fromRatio', renderTarget.width / renderTarget.height);

        this.bindTexture(this.targetTexture, 1);

        this.bindAndDraw(renderTarget);
    }
}
