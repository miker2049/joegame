import 'phaser'
import RainfallPostFX from './RainfallPostFX'
import { WaterDropPostFX } from './WaterDropPostFX'
import { DoomWipePostFX } from './DoomWipePostFX'
import { PageCurlPostFX } from './PageCurlPostFX'
import { WipePostFX } from './WipePostFX'
import PlasmaPostFX from './PlasmaPostFX'
import RexSwirlPostFX from 'phaser3-rex-plugins/plugins/shaders/swirl/SwirlPostFxPipeline'
import { SwirlPipeline } from './SwirlPipeline'
import Blobs from './Blobs'

const shaders = {
  RainfallPostFX,
  WaterDropPostFX,
  DoomWipePostFX,
  PageCurlPostFX,
  WipePostFX,
  // PlasmaPostFX,
  RexSwirlPostFX,
  SwirlPipeline,
  Blobs
}

export default shaders
