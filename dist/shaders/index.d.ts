import 'phaser';
import RainfallPostFX from './RainfallPostFX';
import { WaterDropPostFX } from './WaterDropPostFX';
import { DoomWipePostFX } from './DoomWipePostFX';
import { PageCurlPostFX } from './PageCurlPostFX';
import { WipePostFX } from './WipePostFX';
import PlasmaPostFX from './PlasmaPostFX';
declare const shaders: {
    RainfallPostFX: typeof RainfallPostFX;
    WaterDropPostFX: typeof WaterDropPostFX;
    DoomWipePostFX: typeof DoomWipePostFX;
    PageCurlPostFX: typeof PageCurlPostFX;
    WipePostFX: typeof WipePostFX;
    PlasmaPostFX: typeof PlasmaPostFX;
};
export default shaders;
