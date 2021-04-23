/**
 * @author       Richard Davey <rich@photonstorm.com>
 * @author       laserdog
 * @copyright    2021 Photon Storm Ltd.
 */
import Phaser from 'phaser';
const fragShader = `
#define SHADER_NAME PAGE_CURL_FS

precision mediump float;

uniform sampler2D uMainSampler;
uniform sampler2D uMainSampler2;

uniform int resizeMode;
uniform vec2 resolution;
uniform vec2 from;
uniform vec2 to;
uniform float radius;
uniform float fromRatio;
uniform float toRatio;

varying vec2 outFragCoord;

#define pi 3.14159265359

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

vec4 transition (vec2 p)
{
    float aspect = fromRatio;

    vec2 uv = p * vec2(aspect, 1.0) / resolution.xy;
    vec2 mouse = to * vec2(aspect, 1.0) / resolution.xy;
    vec2 mouseDir = normalize(abs(from) - to);
    vec2 origin = clamp(mouse - mouseDir * mouse.x / mouseDir.x, 0.0, 1.0);
    
    float mouseDist = clamp(length(mouse - origin) 
        + (aspect - (abs(from.x) / resolution.x) * aspect) / mouseDir.x, 0.0, aspect / mouseDir.x);
    
    if (mouseDir.x < 0.0)
    {
        mouseDist = distance(mouse, origin);
    }

    float proj = dot(uv - origin, mouseDir);
    float dist = proj - mouseDist;
    
    vec2 linePoint = uv - dist * mouseDir;
    vec4 color;

    if (dist > radius) 
    {
        color = getToColor(uv * vec2(1.0 / aspect, 1.0));
        color.rgb *= pow(clamp(dist - radius, 0.0, 1.0) * 1.5, 0.2);
    }
    else if (dist >= 0.0)
    {
        //  Map to cylinder point
        float theta = asin(dist / radius);
        vec2 p2 = linePoint + mouseDir * (pi - theta) * radius;
        vec2 p1 = linePoint + mouseDir * theta * radius;
        uv = (p2.x <= aspect && p2.y <= 1.0 && p2.x > 0.0 && p2.y > 0.0) ? p2 : p1;

        color = getFromColor(uv * vec2(1.0 / aspect, 1.0));
        color.rgb *= pow(clamp((radius - dist) / radius, 0.0, 1.0), 0.2);
    }
    else 
    {
        vec2 p = linePoint + mouseDir * (abs(dist) + pi * radius);

        uv = (p.x <= aspect && p.y <= 1.0 && p.x > 0.0 && p.y > 0.0) ? p : uv;

        color = getFromColor(uv * vec2(1.0 / aspect, 1.0));
    }

    return color;
}
    
void main ()
{
    gl_FragColor = transition(gl_FragCoord.xy);
}
`;
export class PageCurlPostFX extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
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
    constructor(game) {
        super({
            game,
            name: 'PageCurlPostFX',
            fragShader
        });
        this.resizeMode = 0;
        this.toRatio = 0;
        this.radius = 0.1;
        this.from = new Phaser.Math.Vector2();
        this.to = new Phaser.Math.Vector2();
    }
    /**
     * @ignore
     */
    onBoot() {
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
     * @memberof PageCurlPostFX
     */
    setResizeMode(mode = 1) {
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
     * @memberof PageCurlPostFX
     */
    setTexture(texture = '__DEFAULT', resizeMode) {
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
    setFrom(x, y) {
        this.from.set(x, y);
        return this;
    }
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
    setTo(x, y) {
        this.to.set(x, y);
        return this;
    }
    /**
     * Sets the radius of the curl effect.
     *
     * Should be a small value. The defaul is 0.1.
     *
     * @param {number} [value=0.1] - The radius.
     * @returns {this}
     * @memberof PageCurlPostFX
    */
    setRadius(value = 0.1) {
        this.radius = value;
        return this;
    }
    /**
     * @ignore
     */
    onPreRender() {
        this.set1i('resizeMode', this.resizeMode);
        this.set1f('radius', this.radius);
    }
    /**
     * @ignore
     */
    onDraw(renderTarget) {
        const w = renderTarget.width;
        const h = renderTarget.height;
        this.set1f('fromRatio', w / h);
        this.set2f('resolution', w, h);
        this.set2f('from', this.from.x, h - this.from.y);
        this.set2f('to', this.to.x, h - this.to.y);
        this.bindTexture(this.targetTexture, 1);
        this.bindAndDraw(renderTarget);
    }
}
//# sourceMappingURL=PageCurlPostFX.js.map