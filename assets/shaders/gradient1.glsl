#ifdef GL_ES
precision mediump float;
#endif

#define PI 3.14159265359

uniform vec2 resolution;
// uniform vec2 u_mouse;
uniform float time;

vec3 colorA = vec3(0.171,0.270,0.912);
//vec3 colorB = vec3(1.000,0.833,0.224);
vec3 colorB = vec3(1.000,0.925,0.212);

float plot (vec2 st, float pct){
  return  smoothstep( pct-0.01, pct, st.y) -
          smoothstep( pct, pct+0.01, st.y);
}

void main( void ) {
    vec2 st = gl_FragCoord.xy/resolution.xy;
    vec3 color = vec3(0.0);
    float t = abs(sin(time/8.));
    vec3 pct = vec3(st.x*t);

     pct.r = smoothstep(0.0,1.0, st.x);
     pct.g = sin(st.x*PI);
     //pct.b = pow(st.x,1.212);

    color = mix(colorA, colorB, pct);

    // Plot transition lines for each channel
    //color = mix(color,colorB,plot(st,pct.r));
    //color = mix(color,vec3(0.0,1.0,0.0),plot(st,pct.g));
    //color = mix(color,vec3(0.0,0.0,1.0),plot(st,pct.b));

    gl_FragColor = vec4(color,1.0);
    // gl_FragColor = vec4(0.0,0.0,1.0,1.0);
}
