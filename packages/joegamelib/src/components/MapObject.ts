import 'phaser'
import { InputType } from 'zlib'
import { ILevelComponents, IMap } from '../ILevel'
import { LevelScene } from '../LevelScene'

export interface ITiledMapObject extends Phaser.Types.Tilemaps.TiledObject {
  depth: number
  texture?: string
}

export interface IMapObject extends Phaser.GameObjects.GameObject {
  name: string
  id: number
  width: number
  height: number
  playAnim(): void
  stopAnim(): void
  x: number
  y: number
}

/**
 * this in itself is a sprite component that gets all of its initialization from
 * the parsed Tiled map object, represented by `IMapObject`
 *
 * @extends Phaser.GameObjects.Sprite
 * @implements IMapObject
 * @property {string} name - the name of the object, which if not defined is "hashed" from start coordinates
 * @property {number} id - id
 * @property {number} tiledWidth
 * @property {number} tiledHeight
 * @property {object} props
 */

export interface IMapObjectConfig {
  name: string
  id: number
  x: number
  y: number
  width: number
  height: number
  texture: string
  frame: number
  flipX: boolean
  flipY: boolean
  depth: number
  rotation: number
  originX: number
  originY: number
  scrollFactor: number
  visible: boolean
  body: boolean
  moveable: boolean
  tint: number
  scene: LevelScene
  popupText: string
}

/*
 * A MapObject is a SpriteMapObject.
 */
export class MapObject extends Phaser.GameObjects.Sprite implements IMapObject {
  name: string
  id: number
  width: number
  height: number

  constructor({
    name,
    id,
    x,
    y,
    width,
    height,
    texture,
    frame,
    flipX,
    flipY,
    depth,
    rotation,
    scrollFactor,
    visible,
    originX,
    originY,
    body,
    moveable,
    tint,
    scene,
    popupText
  }: IMapObjectConfig) {
    super(scene, x, y, texture, frame)

    this.name = name
    this.id = id
    this.width = width
    this.height = height
    this.setSize(this.width, this.height)

    this.setFlipX(flipX)
    this.setFlipY(flipY)
    this.setDepth(depth)
    this.setRotation(Phaser.Math.DegToRad(rotation))
    this.setOrigin(originX, originY)
    this.setDisplaySize(this.width, this.height)
    this.setSize(this.width, this.height)
    if (body) {
      this.scene.physics.world.enableBody(
        this,
        moveable
          ? Phaser.Physics.Arcade.DYNAMIC_BODY
          : Phaser.Physics.Arcade.STATIC_BODY
      )
    }
    this.setScrollFactor(scrollFactor)
    if (tint > 0) {
      this.setTint(tint)
    }
    if (popupText.length > 0) {
      // const raw = this.getData('popupText')
      // const t = Phaser.Display.Color.HexStringToColor(raw.substring(3, 9))
      // this.setTint(t.color)
    }

    // if (level.config.lights) {
    //   this.setPipeline('Light2D')
    // }
    this.setVisible(visible)
    // console.log(`${this.name} is being created!`);
    this.scene.events.addListener(`play_anim_${name}`, () => {
      this.playAnim()
    })
    this.scene.events.addListener(`stop_anim_${name}`, () => {
      this.stopAnim()
    })
    this.scene.events.addListener(this.getData('animHook') || '', () => {
      this.playAnim()
    })

    if (this.getData('playAnim')) {
      this.playAnim()
    }
  }

  playAnim() {
    const anim_ = this.getData('anim')
    if (anim_) {
      this.anims.play(anim_)
      this.setDisplaySize(this.width, this.height)
    } else {
      ;('No anim set on the ${this.name} tiled object (or elsewhere!)')
    }
  }
  stopAnim() {
    this.anims.stop()
  }
}
