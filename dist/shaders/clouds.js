import Phaser from 'phaser';
/* tslint:disable-next-line */
import frag from './clouds.glsl';
const fragShader = frag;
export default class Clouds extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
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
    constructor(game) {
        super({
            game,
            name: 'clouds',
            fragShader
        });
        // this.set1f('RAIN_DENSITY', 0.03)
        // this.set1f('BRIGHTNESS', 0.27)
        // this.set1f('slow', 0.5)
        // this.set1f('gray', 0.1)
    }
    /**
     * @ignore
     */
    onBoot() {
        // this.setTexture();
    }
    onPreRender() {
        this.set1f('iTime', this.game.loop.time / 1000);
    }
    onDraw(renderTarget) {
        // this.set1f('fromRatio', renderTarget.width / renderTarget.height);
        this.set2f('iResolution', renderTarget.width, renderTarget.height);
        // console.log(Math.floor(0.003 * renderTarget.height))
        // this.bindTexture(this.targetTexture, 1);
        this.bindAndDraw(renderTarget);
    }
}
//# sourceMappingURL=clouds.js.map