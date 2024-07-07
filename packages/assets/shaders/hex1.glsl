#version 300 es
// Hexagon Tiled Audio Shader
//
// Just playing around after getting some help with hexagon
// tiling from @Shane https://www.shadertoy.com/view/3d2fzK
// learning though deconstruction

#ifdef GL_ES
precision mediump float;
#endif

uniform vec2 resolution;
// uniform vec2 u_mouse;
uniform float time;
#define MAX_DIST 	135.

#define PI  		3.1415926
#define PI2 		6.2831952
#define R 			resolution
// #define M 			iMouse
#define T 			time
#define S 			smoothstep
#define r2(a)  mat2(cos(a), sin(a), -sin(a), cos(a))

float sampleFreq(float freq) {return 0.5;}
//@iq vec2 to float hash.
float hash2(vec2 p){  return fract(sin(dot(p, vec2(27.609, 57.583)))*43758.5453); }
// // not used
// vec3 get_mouse( vec3 ro ) {
//   float x = M.xy==vec2(0) ? -.6 : -(M.y / R.y * 1. - .5) * PI;
//   float y = M.xy==vec2(0) ? .9 : (M.x / R.x * 2. - 1.) * PI;
//   ro.zy *= r2(x);
//   ro.zx *= r2(y);
//   return ro;
// }

// my lazy global v's
float mid = 0.;
vec3 hitPoint =vec3(0.);

#define HEX_SCALE 1.95
//@Shane - Hexagon tiling >mind blown<
vec4 map(vec3 q3){
  q3.xz*=r2(T*.22);
  // Scaling and Vars
  const float scale = 2./HEX_SCALE;
  const vec2 l = vec2(scale*1.732/2., scale);
  const vec2 s = l*2.;
  float d = 1e5;
  vec2 p, ip;
  // IDs and Center Points
  vec2 id = vec2(0);
  vec2 cntr = vec2(0);
  const vec2[4] ps4 = vec2[4](vec2(-l.x, l.y), l + vec2(0., l.y), -l, vec2(l.x, -l.y) + vec2(0., l.y));
  // which pass you're on
  float boxID = 0.;
  for(int i = 0; i<4; i++){
    // Block center.
    cntr = ps4[i]/2.;
    // Local coordinates.
    p = q3.xz - cntr;
    ip = floor(p/s) + .5; // Local tile ID.
    p -= (ip)*s; // New local position.
    // Correct positional individual tile ID.
    vec2 idi = (ip)*s + cntr;
    //float hx=hash2(idi*.5);
    float hx=distance(idi,vec2(.0));
    float th = sampleFreq(.01+hx*.0021)*45.;
    th = abs(th*14.999)/15.*.15;
    // make shape
    vec3 p3 = vec3(p.x,q3.y,p.y);
    float sp = length(p3-vec3(0.,th,0.))-((th*.12));
    if(sp<d){
      d = sp;
      id = idi;
      boxID = float(i);
      mid = 2.;
    }
  }
  // Return the distance, position-base ID and box ID.
  return vec4(d/1.7, id, boxID);
}

//Tetrahedron technique
vec3 get_normal(in vec3 p, in float t) {
  //https://iquilezles.org/www/articles/normalsSDF/normalsSDF.htm
  float h = 0.0002*t;
#define ZERO (min(0,0))
  vec3 n = vec3(0.0);
  for( int i=ZERO; i<4; i++ ){
    vec3 e = 0.5773*(2.0*vec3((((i+3)>>1)&1),((i>>1)&1),(i&1))-1.0);
    n += e*map(p+e*h).x;
  }
  return normalize(n);
}

vec4 ray_march( in vec3 ro, in vec3 rd, int maxstep ) {
  float t = 0.0001;
  vec3 m = vec3(0.);
  for( int i=0; i<maxstep; i++ ) {
    vec4 d = map(ro + rd * t);
    m = d.yzw;
    if(d.x<.001*t||t>MAX_DIST) break;
    t += d.x*.5;
  }
  return vec4(t,m);
}

float get_diff(in vec3 p, in vec3 lpos, in vec3 n) {
  vec3 l = lpos-p;
  vec3 lp = normalize(l);
  float dif = clamp(dot(n,lp),0. , 1.),
    shadow = ray_march(p + n * 0.0002 * 2.,lp,128).x;
  if(shadow < length(l)) dif *= .2;
  return dif;
}


void mainImage( out vec4 O, in vec2 F ) {
  vec2 U = (2.*F.xy-R.xy)/max(R.x,R.y);
  vec3 ro = vec3(0.,11.,12.5),
    lp = vec3(0.,4.,.0);

  //ro = get_mouse(ro);
  float tmod = mod(T*.15,3.);

  if(tmod<1.) {
    lp = vec3(.0,0.,.0);
    ro = vec3(.0,15.,1.);
  } else if(tmod<2.) {
    ro = vec3(0.,8.,22.5);
    lp = vec3(0.,5.,.0);
  }
  vec3 cf = normalize(lp-ro),
    cp = vec3(0.,1.,0.),
    cr = normalize(cross(cp, cf)),
    cu = normalize(cross(cf, cr)),
    c = ro + cf * .85,
    i = c + U.x * cr + U.y * cu,
    rd = i-ro;

  vec3 C = vec3(0.);
  // trace dat map
  vec4 ray = ray_march(ro,rd,256);
  float t = ray.x;
  vec3 hid = ray.yzw;
  if(t<MAX_DIST) {
    vec3 p = ro + t * rd,
      n = get_normal(p, t),
      h = vec3(.5);
    float dst = distance(hid.xy,vec2(.0));

    float cht = 22.;
    h = .5 + .45*cos(PI2*dst/cht + vec3(0, 1, 2));
    vec3 lpos1 = vec3(.5, 25.0, -.5),
      diff =  vec3(1.) * get_diff(p, lpos1, n);
    C += h * (diff);

    // single bounce reflect
    vec3 rr=reflect(rd,n);
    vec4 ref=ray_march(p,rr,128);
    hid = ref.yzw;
    if(ref.x<MAX_DIST){
      dst = distance(hid.xy,vec2(.0));
      p+=ref.x*rr;
      n = get_normal(p, ref.x),
        h = .5 + .45*cos(PI2*dst/cht + vec3(0, 1, 2));
      diff = vec3(.7)*get_diff(p, lpos1, n);
      C += h * diff ;
    }
  } else {
    C += vec3(.025);
  }
  // fog and gamma out - cheap
  vec3 FD = mix(vec3(.07),vec3(.4,.3,.21),U.y);
  C = mix( C, FD, 1.-exp(-.00005*t*t*t));
  O = vec4(pow(C, vec3(0.4545)),1.0);
}
