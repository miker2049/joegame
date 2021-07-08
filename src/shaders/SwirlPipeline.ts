import 'phaser'

const SinglePipeline = Phaser.Renderer.WebGL.Pipelines.SinglePipeline;
const GetValue = Phaser.Utils.Objects.GetValue;
const DegToRad = Phaser.Math.DegToRad;
const RadToDeg = Phaser.Math.RadToDeg;

// reference : https://www.geeks3d.com/20110428/shader-library-swirl-post-processing-filter-in-glsl/

const frag = `\
#ifdef GL_FRAGMENT_PRECISION_HIGH
#define highmedp highp
#else
#define highmedp mediump
#endif
precision highmedp float;

// Scene buffer
uniform sampler2D uMainSampler;
varying vec2 outTexCoord;
varying float outTintEffect;
varying vec4 outTint;

// Effect parameters
uniform vec2 texSize;
uniform vec2 center;
uniform float radius;
uniform float angle;

void main (void) {
  vec2 tc = outTexCoord * texSize;
  tc -= center;
  float dist = length(tc);
  if (dist < radius) {
    float percent = (radius - dist) / radius;
    float theta = percent * percent * angle * 8.0;
    float s = sin(theta);
    float c = cos(theta);
    tc = vec2(dot(tc, vec2(c, -s)), dot(tc, vec2(s, c)));
  }
  tc += center;
  vec4 texture = texture2D(uMainSampler, tc / texSize);
  vec4 texel = vec4(outTint.bgr * outTint.a, outTint.a);
        //  Multiply texture tint,
        vec4 color = texture * texel;
        if (outTintEffect == 1.0)
        {
            //  Solid color + texture alpha
            color.rgb = mix(texture.rgb, outTint.bgr * outTint.a, texture.a);
        }
        else if (outTintEffect == 2.0)
        {
            //  Solid color, no texture,
            color = texel;
        }
        gl_FragColor = color;
}
`;

class SwirlPipeline extends SinglePipeline {
    centerX: number;
    centerY: number;
    radius: number;
    rotation: number;

    constructor(game) {
        super({
            name: 'swirlPipeline',
            game: game,
            renderTarget: true,
            fragShader: frag
        });

        this.centerX = 0; // position wo resolution
        this.centerY = 0; // position wo resolution
        this.radius = 0;
        this.rotation = 0;
    }

    resetFromJSON(o) {
        this.radius = GetValue(o, 'radius', 0);
        var rotation = GetValue(o, 'rotation', undefined);
        if (rotation === undefined) {
            this.setAngle(GetValue(o, 'angle', 0));
        } else {
            this.setRotation(rotation);
        }
        this.setCenter(GetValue(o, 'center.x', undefined), GetValue(o, 'center.y', undefined));
        return this;
    }

    onPreRender() {
        this.set1f('radius', this.radius);
        this.set1f('angle', this.rotation);

        var texWidth = this.renderer.width,
            textHeight = this.renderer.height;
        this.set2f('center', this.centerX, (textHeight - this.centerY));
        this.set2f('texSize', texWidth, textHeight);
    }

    // radius
    setRadius(value) {
        this.radius = value;
        return this;
    }

    // rotation
    setRotation(value) {
        this.rotation = value;
        return this;
    }

    get angle() {
        return RadToDeg(this.rotation);
    }

    set angle(value) {
        this.rotation = DegToRad(value);
    }

    setAngle(value) {
        this.angle = value;
        return this;
    }

    // center
    setCenter(x, y) {
        if (x === undefined) {
            x = this.renderer.width / 2;
            y = this.renderer.height / 2;
        }
        this.centerX = x;
        this.centerY = y;
        return this;
    }
}

export default SwirlPipeline;
