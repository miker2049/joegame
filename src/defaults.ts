import { ILevelConfig } from "./ILevelConfig"

const levelConfig: ILevelConfig = {
    objectLayers: ['Objects', 'aboveObjects'],
    platformLayers: ['Platforms'],
    npcLayers: ['NPCs'],
    convosLayers: 'TweetConvos',
    zoom: 4,
}

const emitterConfig = {
    // **basic properties of particles**
    // **initial position**
    // x: 0,             // { min, max }, or { min, max, steps }
    // y: 0,             // { min, max }, or { min, max, steps }
    // follow: null,
    // followOffset: {
    //    x: 0,
    //    y: 0
    // },
    // **emit zone**
    // emitZone: {
    //     type: 'random',    // 'random', or 'edge'
    //     source: geom,      // Geom like Circle, or a Path or Curve
    //     **type = edge**
    //     quantity: 1,
    //     stepRate: 0,
    //     yoyo: false,
    //     seamless: true
    // },

    // **target position**
    // moveToX:          // { min, max }, or { min, max, steps }
    // moveToY:          // { min, max }, or { min, max, steps }
    // **death zone**
    // deathZone: {
    //      type: 'onEnter',  // 'onEnter', or 'onLeave'
    //      source: geom      // Geom like Circle or Rect that supports a 'contains' function
    // }

    // **angle**
    radial: true,
    angle: { min: 0, max: 360 },  // { start, end, steps }

    // **scale**
    // scale: 1,             // { start, end },
    // scaleX: 1,
    // scaleY: 1,

    // **render**
    // frame:                // one or more texture frames, or a configuration object.
    // alpha: 1,             // { min, max }
    // visible: true,
    // tint: 0xffffffff,     // a number 0xfffffff, or an array [ 0xffff00, 0xff0000, 0x00ff00, 0x0000ff ]
    // blendMode: 'NORMAL',  // Phaser.BlendModes

    // delay: 0,
    // lifespan: 1000,       // { min, max }, or { min, max, steps }

    // **physics**
    // speed:                // { min, max }, or { min, max, steps }
    // speedX:               // { min, max }, or { min, max, steps }
    // speedY:               // { min, max }, or { min, max, steps }
    // gravityX:
    // gravityY:
    // accelerationX:
    // accelerationY:
    // maxVelocityX: 10000,
    // maxVelocityY: 10000,

    // **bounce**
    // bounce: 0,
    // bounds: nul,           // Phaser.Geom.Rectangle, or { x, y, width, height }
    // collideBottom: true,
    // collideTop: true,
    // collideLeft: true,
    // collideRight : true,

    // **callback**
    // emitCallback: null,
    // emitCallbackScope: null,
    // deathCallback: null,
    // deathCallbackScope: null,

    // **custom particle**
    // particleClass: Phaser.GameObjects.Particles.Particle

    // **emitter**
    // name: '',
    // on: true,          // set false to stop emitter
    // active: true,      // set false to pause emitter and particles
    // frequency: 0,      // -1 for exploding emitter
    // quantity: 1,       // { min, max }
    // maxParticles: 0,
    // reserve: 0,
    // rotate: 0,         // { start, end }, or { start, end, ease }
    // timeScale: 1,
}


export default {
    scale: 1,
    animLength: 3,
    dashDistance: 32,
    speed: 16,
    globalDrag: 1,
    tileWidth: 16,
    tileHeight: 16,
    charDepth: 2,
    platformDepth: 10,
    patience: 1000,
    charAccel: 128,
    talkingSpeed: 45,
    emoji: 'sweat',
    texture: 'default',
    emojiPath: 'assets/images/emoji/',
    soundURL: 'assets/audio/sounds/walk.mp3',
    levelConfig,
    emitterConfig
}
