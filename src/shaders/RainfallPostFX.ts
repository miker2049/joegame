import Phaser from 'phaser';
/* tslint:disable-next-line */
import frag from './RainfallPostFX.glsl'
const fragShader = frag

export default class WaterDropPostFX extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {

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
            name: 'RainfallPostFX',
            fragShader: `
precision mediump float;
uniform float RAIN_DENSITY;
uniform float BRIGHTNESS;        // raindrop brightness contrast
const float BLUR_LENGTH = 30.;        // max length of raindrop blured line
const float SPEED = 500.;

#define rnd(p,s)   fract(sin( (p+(.01*s)) *12.9898) * 43758.5453)
uniform vec2 iResolution;

uniform sampler2D uMainSampler;
uniform float iTime;

varying vec2 outFragCoord;

void main()
    {
    vec2 R = iResolution.xy;
    vec4 txt = texture2D(uMainSampler, outFragCoord);
    vec2 U = gl_FragCoord.xy;
    U -= .5;
    U.x += 1.5;
    vec4 O = txt;

    float Ny = RAIN_DENSITY * R.y;            // number of drop per column
    float LIM = floor(Ny);
    for (float i=0.0; i<=0.; i++) {     // to deal with more than one drop per column
        float y = floor( mod( rnd(U.x,2.*i)*R.y -SPEED*iTime, R.y) ); // drop altitude
        if ( rnd(U.x,2.*i+1.) < (Ny-i) && abs( U.y - y) < BLUR_LENGTH*(U.x/R.x) )
            O += vec4(0.,0.,BRIGHTNESS,0.); // / (U.x/R.x);  //  / (U.x/R.x); // variant: keep total drop brightness. attention: saturated on the left 5%
    }


   // O = sqrt(O);                              // gamma correction

//    O.rgb += col.rgb * 0.5;

    gl_FragColor = O;
}`
        });

        this.set1f('RAIN_DENSITY', 0.03)
        this.set1f('BRIGHTNESS', 0.27)
        // this.set1f('slow', 0.5)
        // this.set1f('gray', 0.1)
        console.log('wjattt')
    }

    /**
     * @ignore
     */
    onBoot(): void {
        // this.setTexture();
    }

    onPreRender() {
        this.set1f('iTime', this.game.loop.time / 1000);
    }

    onDraw(renderTarget: Phaser.Renderer.WebGL.RenderTarget): void {
        // this.set1f('fromRatio', renderTarget.width / renderTarget.height);

        this.set2f('iResolution', renderTarget.width, renderTarget.height);
        // console.log(Math.floor(0.003 * renderTarget.height))
        // this.bindTexture(this.targetTexture, 1);

        this.bindAndDraw(renderTarget);
    }
}
