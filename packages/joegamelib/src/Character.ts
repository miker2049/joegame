import 'phaser'
import jumpUp from './actions/charJumpUp'
import createResidualGraphic from './actions/createResidualGraphic'
import NameLabel from './components/NameLabel'
import { createNPCMachine } from './components/NPCMachine'
// import VoxBox from './components/VoxBox'
import VoxBox from './components/VoxBox'
import defaults from './defaults'
import { CharacterConfig } from './ICharacter'
import {
  CharMoveAnims,
  Dir,
  GameObjectInWorld,
  VelocityMap
} from './joegameTypes'
import { LevelScene } from './LevelScene'
import getDashVelForDistance from './utils/getDashVelForDistance'

export const undefinedOrNegOne = (n: number) => n === undefined || n === -1
export const valOrDefault = (n: number, def: number) =>
  undefinedOrNegOne(n) ? def : n

export default class Character extends Phaser.Physics.Arcade.Sprite {
  //settings
  speed: number
  readonly name: string
  dashDistance: number
  charge: number
  id: string
  readonly animKeys: CharMoveAnims

  //private seetings
  dashDrag: number
  dashVel: number
  voxbox: VoxBox

  //state
  public depth: number
  public facing: Dir
  public onPlatform: boolean
  public auto: boolean
  public player: boolean
  public groundVel: Phaser.Types.Math.Vector2Like

  declare scene: LevelScene
  declare type: 'Character'
  declare body: Phaser.Physics.Arcade.Body

  // level: ILevelComponents

  constructor(config: CharacterConfig) {
    super(config.scene, config.x, config.y, config.texture)
    this.type = 'Character'

    //assign settings from config
    this.speed = config.speed
    this.name = config.name
    this.dashDistance = config.dashDistance
    this.charge = 2
    try {
      this.animKeys = {
        north: config.anims.north.split(',').map((i) => parseInt(i)),
        south: config.anims.south.split(',').map((i) => parseInt(i)),
        east: config.anims.east.split(',').map((i) => parseInt(i)),
        west: config.anims.west.split(',').map((i) => parseInt(i))
      }
    } catch (e) {
      throw Error('issue parsing animKey config for ' + this.name)
    }
    this.dashDrag = 300
    this.dashVel = getDashVelForDistance(this.dashDistance, this.dashDrag)
    this.depth = config.depth || defaults.charDepth
    this.setDepth(this.depth)
    this.face(Dir.south)
    this.facing = Dir.south
    this.onPlatform = false
    this.auto = false
    this.player = false
    this.groundVel = { x: 0, y: 0 }
    this.setScale(config.scale)
    this.setOrigin(0.5, 0.5)
    this.setInteractive(
      new Phaser.Geom.Circle(0, 0, this.scene.map.tileWidth * 2),
      Phaser.Geom.Circle.Contains
    )
    this.scene.add.existing(this)
    this.scene.physics.add.existing(this, false)
    this.body.setSize(
      valOrDefault(config.body?.width || -1, this.scene.map.tileWidth * 0.5),
      valOrDefault(config.body?.height || -1, this.scene.map.tileHeight * 0.5)
    )
    // This is in general a good heuristic for setting the body offset,
    // but TODO, read from config as well
    this.body.setOffset(
      this.texture.get(0).width / 2 - this.body.width / 2,
      this.texture.get(0).height - this.body.height
    )
    // Create anims
    Object.keys(this.animKeys).forEach((k) => {
      this.anims.create({
        key: k,
        frames: this.animKeys[k as keyof typeof this.animKeys].map((i) => ({
          key: config.texture,
          frame: i
        })),
        frameRate: 10,
        repeat: -1
      })
    })
    this.id = this.name
    // this.charBody.setOffset(config.body?.offsetX || 0, config.body?.offsetY || 0)
    this.voxbox = new VoxBox(this as GameObjectInWorld)

    const nameLabel = new NameLabel(
      this.scene,
      this.name,
      this as GameObjectInWorld
    )
    this.scene.add.existing(nameLabel)
    // this.add(nameLabel)
    this.on('pointerover', () => {
      nameLabel.open()
    })
    this.on('pointerout', () => {
      nameLabel.close()
    })

    // this.sprite.on('animationstart', (anim, frame) => { console.log(`start of ${anim}`) })
    // this.sprite.on('animationrepeat', (anim, frame) => { console.log(`update of ${anim}`) })
    // this.sprite.on('animationupdate', (anim, frame) => {
    //     console.log(`is ${anim}, frame is ${frame}`)
    // })

    // Gen NPC interests
    const tts = this.scene.map.getTilesWithinShape(
      new Phaser.Geom.Circle(this.x, this.y, 16 * 1),
      undefined,
      undefined,
      'COLLIDERS'
    )
    const mach = createNPCMachine(
      this,
      this.scene.map.tileWidth,
      this.scene.pathfinder,
      tts
        .filter((ti) => ti.index - ti.tileset.firstgid === 26)
        .map((ti) => ({ x: ti.pixelX, y: ti.pixelY }))
        .sort((_a, _b) => Math.random() * 2 - 1)
    )
    this.scene.machineRegistry.add('npc_' + this.name, mach)
  }

  //control
  changeGroundVel(): void {
    // let platform: Phaser.Types.Physics.Arcade.GameObjectWithDynamicBody
    // this.scene.physics.world.overlap(
    //   this,
    //   this.level.platforms,
    //   (player, plat) => {
    //     platform = plat as Phaser.Types.Physics.Arcade.GameObjectWithDynamicBody
    //     if (platform != undefined) {
    //       this.groundVel = {
    //         x: platform.body.velocity.x,
    //         y: platform.body.velocity.y
    //       }
    //     } else {
    //       this.groundVel = { x: 0, y: 0 }
    //     }
    //   }
    // )
  }
  move(dir: Dir): void {
    this.body.allowDrag = false
    this.facing = dir
    this.playAnim(Dir[dir])
    this.body.setVelocity(this.groundVel.x || 0, this.groundVel.y || 0) // this is needed again to cancel diagonole shit
    this.body.velocity.add(
      new Phaser.Math.Vector2(
        this.speed * VelocityMap[dir][0],
        this.speed * VelocityMap[dir][1]
      )
    )
  }
  dash(dir: Dir): void {
    this.showDashboxes()
    this.body.allowDrag = true
    this.body.setDrag(this.dashDrag, this.dashDrag)
    this.body.setVelocity(0)
    this.body.setAcceleration(0, 0)
    this.face(dir)
    this.stopAnim()
    this.body.setVelocity(
      this.dashVel * VelocityMap[dir][0] + (this.groundVel.x || 0),
      this.dashVel * VelocityMap[dir][1] + (this.groundVel.y || 0)
    )
    createResidualGraphic(this, this.x + this.x, this.y + this.y)
  }

  face(dir: Dir): void {
    const dirAnim = this.anims.get(Dir[dir])
    if (dirAnim) {
      this.anims.setCurrentFrame(dirAnim.frames[1])
      this.facing = dir
    }
  }

  stopp(face?: Dir): void {
    this.body.allowDrag = false
    this.body.setVelocity(this.groundVel.x || 0, this.groundVel.y || 0)
    this.stopAnim()
    if (face) {
      this.face(face)
      this.facing = face
    }
  }
  align(): Phaser.Types.Math.Vector2Like {
    if (!this.scene.map) return { x: undefined, y: undefined }
    const x_ =
      Math.floor(this.body.center.x / this.scene.map.tileWidth) *
      this.scene.map.tileWidth
    const y_ =
      Math.floor(this.body.center.y / this.scene.map.tileHeight) *
      this.scene.map.tileHeight
    this.transport(
      x_ + this.scene.map.tileWidth / 2,
      y_ + this.scene.map.tileHeight / 2
    )
    // this.transport(x_, y_)
    return {
      x: x_ / this.scene.map.tileWidth,
      y: y_ / this.scene.map.tileHeight
    }
  }

  jumpUp(): void {
    jumpUp(this)
  }

  /*
   * The dir here marks the direction where you are jumping back from
   */
  jumpBack(dir: Dir): void {
    // console.log(this.name + " is jumping!")
    this.jumpUp()
    switch (dir) {
      //if collider.body.touching.up
      case Dir.north: {
        this.face(Dir.north)
        this.transportNudge(0, 8)
        this.align()
        break
      }
      case Dir.south: {
        this.face(Dir.south)
        this.transportNudge(0, -8)
        this.align()
        break
      }
      case Dir.east: {
        this.face(Dir.east)
        this.transportNudge(-8, 0)
        this.align()
        break
      }
      case Dir.west: {
        this.face(Dir.west)
        this.transportNudge(8, 0)
        this.align()
        break
      }
    }
  }

  transport(x: number, y: number): void {
    const point = this.body?.center || this.getCenter()
    this.setPosition(x + (this.x - point.x), y + (this.y - point.y))
  }
  transportNudge(x: number, y: number): void {
    this.setPosition(this.x + x, this.y + y)
  }

  minusCharge(): void {}
  playAnim(anim: string): void {
    if (this.anims) {
      this.anims.play(
        { key: anim, delay: 0, repeat: -1, frameRate: 11, startFrame: 1 },
        false
      )
    }
  }
  stopAnim(): void {
    if (this.anims) {
      this.anims.stop()
      const dirAnim = this.anims.get(Dir[this.facing])
      this.anims.setCurrentFrame(dirAnim.frames[1])
    }
  }

  // UI control
  showDashboxes(): void {}
  minusDashbox(): void {}
  showLabel(): void {}
  hideLabel(): void {}

  async speak(msg: string): Promise<void> {
    await Promise.all([
      this.voxbox.speak(msg)
      // TODO when we get speaking back
      // speakString(msg, this, (config: ITalkingPlayConfig): void => Phaser.Utils.NOOP())
    ])
    return

    // console.log(msg)
  }
}
