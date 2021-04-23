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
    constructor(game) {
        super({
            game,
            name: 'blobs',
            fragShader: `
// Blobs by @paulofalcao

precision mediump float;


uniform vec2 iResolution;
uniform sampler2D uMainSampler;
uniform float iTime;
uniform float scale;

varying vec2 outFragCoord;

#define time iTime

float makePoint(float x,float y,float fx,float fy,float sx,float sy,float t){
   float xx=x+sin(t*fx)*sx;
   float yy=y+cos(t*fy)*sy;
   return 1.0/sqrt(xx*xx+yy*yy);
}
void blobber(vec2 pos){
  vec4 fragColor = texture2D(uMainSampler, outFragCoord);
  vec2 fragCoord = gl_FragCoord.xy;
   vec2 p=(fragCoord.xy/iResolution.x)*2.0-vec2(1.0,iResolution.y/iResolution.x);

   p=p*8.0*scale;

    vec2 pp = vec2(-2.0*pos.x,-2.0*pos.y) + (2.0 * gl_FragCoord.xy / iResolution.xy);
   pp=pp*8.0*scale;
   float x=pp.x;
   float y=pp.y;
// x += iResolution.x*0.125;
// y -= 2.1;
   float a=
       makePoint(x,y,3.3,2.9,0.3,0.3,time);
   a=a+makePoint(x,y,1.9,2.0,0.4,0.4,time);
   a=a+makePoint(x,y,0.8,0.7,0.4,0.5,time);
   a=a+makePoint(x,y,2.3,0.1,0.6,0.3,time);
   a=a+makePoint(x,y,0.8,1.7,0.5,0.4,time);
   a=a+makePoint(x,y,0.3,1.0,0.4,0.4,time);
   a=a+makePoint(x,y,1.4,1.7,0.4,0.5,time);
   a=a+makePoint(x,y,1.3,2.1,0.6,0.3,time);
   a=a+makePoint(x,y,1.8,1.7,0.5,0.4,time);

   float b=
       makePoint(x,y,1.2,1.9,0.3,0.3,time);
   b=b+makePoint(x,y,0.7,2.7,0.4,0.4,time);
   b=b+makePoint(x,y,1.4,0.6,0.4,0.5,time);
   b=b+makePoint(x,y,2.6,0.4,0.6,0.3,time);
   b=b+makePoint(x,y,0.7,1.4,0.5,0.4,time);
   b=b+makePoint(x,y,0.7,1.7,0.4,0.4,time);
   b=b+makePoint(x,y,0.8,0.5,0.4,0.5,time);
   b=b+makePoint(x,y,1.4,0.9,0.6,0.3,time);
   b=b+makePoint(x,y,0.7,1.3,0.5,0.4,time);

   float c=
       makePoint(x,y,3.7,0.3,0.3,0.3,time);
   c=c+makePoint(x,y,1.9,1.3,0.4,0.4,time);
   c=c+makePoint(x,y,0.8,0.9,0.4,0.5,time);
   c=c+makePoint(x,y,1.2,1.7,0.6,0.3,time);
   c=c+makePoint(x,y,0.3,0.6,0.5,0.4,time);
   c=c+makePoint(x,y,0.3,0.3,0.4,0.4,time);
   c=c+makePoint(x,y,1.4,0.8,0.4,0.5,time);
   c=c+makePoint(x,y,0.2,0.6,0.6,0.3,time);
   c=c+makePoint(x,y,1.3,0.5,0.5,0.4,time);

   vec3 d=vec3(a,b,c)/32.0;

   fragColor += vec4(d.x,d.y,d.z,(d.x+d.y+d.z)/3.0);
   gl_FragColor = fragColor;
}

void main() {
  blobber(vec2(0.75,0.25));
}
`
        });
        // this.set1f('RAIN_DENSITY', 0.03)
        // this.set1f('BRIGHTNESS', 0.27)
        // this.set1f('slow', 0.5)
        // this.set1f('gray', 0.1)
        console.log('wjattt');
    }
    /**
     * @ignore
     */
    onBoot() {
        // this.setTexture();
        this.set1f('scale', 1);
    }
    onPreRender() {
        this.set1f('iTime', this.game.loop.time / 1000);
    }
    onDraw(renderTarget) {
        // this.game.phy
        this.set2f('iResolution', renderTarget.width, renderTarget.height);
        this.bindAndDraw(renderTarget);
    }
}
//# sourceMappingURL=Blobs.js.map