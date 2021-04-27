/**
 * @author       Richard Davey <rich@photonstorm.com>
 * @author       Zeh Fernando
 * @copyright    2021 Photon Storm Ltd.
 */

import Phaser from 'phaser';

const fragShader = `
#define SHADER_NAME DOOMWIPE_FS

precision mediump float;

uniform sampler2D uMainSampler;
uniform sampler2D uMainSampler2;

uniform int resizeMode;
uniform float progress;
uniform float fromRatio;
uniform float toRatio;

varying vec2 outFragCoord;

//  Transition specific
uniform int bars;
uniform float amplitude;
uniform float noise;
uniform float frequency;
uniform float dripScale;

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

// Transition Author: Zeh Fernando
// Transition License: MIT

float rand (int num)
{
    return fract(mod(float(num) * 67123.313, 12.0) * sin(float(num) * 10.3) * cos(float(num)));
}
  
float wave (int num)
{
    float fn = float(num) * frequency * 0.1 * float(bars);
    return cos(fn * 0.5) * cos(fn * 0.13) * sin((fn+10.0) * 0.3) / 2.0 + 0.5;
}
  
float drip (int num)
{
    return sin(float(num) / float(bars - 1) * 3.141592) * dripScale;
}
  
float pos (int num)
{
    return (noise == 0.0 ? wave(num) : mix(wave(num), rand(num), noise)) + (dripScale == 0.0 ? 0.0 : drip(num));
}
  
vec4 transition (vec2 uv)
{
    int bar = int(uv.x * (float(bars)));
    float scale = 1.0 + pos(bar) * amplitude;
    float phase = progress * scale;
    float posY = uv.y / vec2(1.0).y;

    vec2 p;
    vec4 c;

    if (phase + posY < 1.0)
    {
        p = vec2(uv.x, uv.y + mix(0.0, vec2(1.0).y, phase)) / vec2(1.0).xy;
        c = getFromColor(p);
    }
    else
    {
        p = uv.xy / vec2(1.0).xy;
        c = getToColor(p);
    }

    // Finally, apply the color
    return c;
}

void main ()
{
    vec2 uv = outFragCoord;

    gl_FragColor = transition(uv);
}
`;

export class DoomWipePostFX extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
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
    constructor(game: Phaser.Game) {
        super({
            game,
            name: 'DoomWipePostFX',
            fragShader
        });

        this.progress = 0;
        this.resizeMode = 1;
        this.toRatio = 0;

        this.targetTexture = {}
        this.bars = 30;
        this.amplitude = 2;
        this.noise = 0.1;
        this.frequency = 0.5;
        this.dripScale = 0.5;
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
     * @memberof DoomWipePostFX
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
     * @memberof DoomWipePostFX
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
     * @memberof DoomWipePostFX
     */
    setProgress(value: number = 0): this {
        this.progress = Phaser.Math.Clamp(value, 0, 1);

        return this;
    }

    /**
     * Sets the number of bars / columns that drop down.
     *
     * @param {number} [value=30] - The number of bars / columns.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setBars(value: number = 30): this {
        this.bars = value;

        return this;
    }

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
    setAmplitude(value: number = 2): this {
        this.amplitude = value;

        return this;
    }

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
    setNoise(value: number = 0.1): this {
        this.noise = value;

        return this;
    }

    /**
     * Horizontal speed variation.
     *
     * The bigger the value, the shorter the waves.
     *
     * @param {number} [value=0.5] - The frequency.
     * @returns {this}
     * @memberof DoomWipePostFX
     */
    setFrequency(value: number = 0.5): this {
        this.frequency = value;

        return this;
    }

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
    setDripScale(value: number = 0.5): this {
        this.dripScale = value;

        return this;
    }

    /**
     * @ignore
     */
    onPreRender(): void {
        this.set1f('progress', this.progress);
        this.set1i('resizeMode', this.resizeMode);

        this.set1i('bars', this.bars);
        this.set1f('amplitude', this.amplitude);
        this.set1f('noise', this.noise);
        this.set1f('frequency', this.frequency);
        this.set1f('dripScale', this.dripScale);
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
