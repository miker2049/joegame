import 'phaser'
import { interpret } from 'xstate'
import jumpUp from './actions/charJumpUp'
import createResidualGraphic from './actions/createResidualGraphic'
import { createNPCMachine } from './components/NPCMachine'
// import NameLabel from './components/NameLabel'
// import VoxBox from './components/VoxBox'
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

const TILEWIDTH = 16

export default class Character extends Phaser.GameObjects.Container {
  //settings
  speed: number
  readonly name: string
  dashDistance: number
  charge: number
  readonly animKeys: CharMoveAnims

  //private seetings
  dashDrag: number
  dashVel: number
  sprite: Phaser.GameObjects.Sprite
  // voxbox: VoxBox

  //state
  public depth: number
  public facing: Dir
  public onPlatform: boolean
  public auto: boolean
  public player: boolean
  public groundVel: Phaser.Types.Math.Vector2Like

  charBody: Phaser.Physics.Arcade.Body
  declare scene: LevelScene
  declare type: 'Character'

  // level: ILevelComponents

  constructor(config: CharacterConfig) {
    super(config.scene, config.x, config.y)
    this.type = 'Character'
    // this.level = config.level
    //assign settings from config
    this.speed = config.speed
    this.name = config.name
    this.dashDistance = config.dashDistance
    this.charge = 2
    this.animKeys = {
      north: config.anims.north.split(',').map((i) => parseInt(i)),
      south: config.anims.south.split(',').map((i) => parseInt(i)),
      east: config.anims.east.split(',').map((i) => parseInt(i)),
      west: config.anims.west.split(',').map((i) => parseInt(i))
    }
    this.dashDrag = 300
    this.dashVel = getDashVelForDistance(this.dashDistance, this.dashDrag)
    this.sprite = config.scene.make.sprite({ key: config.texture }, false)
    this.sprite.setOrigin(0.5)
    this.sprite.removeFromDisplayList()
    // this.voxbox = new VoxBox(this.level)
    // this.voxbox.close()
    this.depth = config.depth || defaults.charDepth
    this.setDepth(this.depth)
    this.face(Dir.south)
    this.facing = Dir.south
    this.onPlatform = false
    this.auto = false
    this.player = false
    this.groundVel = { x: 0, y: 0 }

    // this.sprite.setTintFill(Phaser.Display.Color.RandomRGB().color)
    // if (this.level.config.lights) {
    //   this.sprite.setPipeline('Light2D')
    // }
    // this.setSize(this.scene.tileWidth/2,this.scene.tileHeight/2)
    this.sprite.setScale(config.scale)
    this.setInteractive(
      new Phaser.Geom.Circle(0, 0, TILEWIDTH * 2),
      Phaser.Geom.Circle.Contains
    )

    this.scene.physics.world.enable(this, Phaser.Physics.Arcade.DYNAMIC_BODY)
    this.charBody = this.body as Phaser.Physics.Arcade.Body

    // this.charBody.setSize(
    //   config.body?.width || TILEWIDTH * 0.5,
    //   config.body?.height || TILEWIDTH * 0.5
    // )

    this.sprite.setPosition(
      this.charBody.halfWidth + -1 * (config.body?.offsetX || 0),
      this.charBody.halfHeight + -1 * (config.body?.offsetY || 0)
    )
    this.add(this.sprite)

    Object.keys(this.animKeys).forEach((k) => {
      this.sprite.anims.create({
        key: k,
        frames: this.animKeys[k as keyof typeof this.animKeys].map((i) => ({
          key: config.texture,
          frame: i
        })),
        frameRate: 10,
        repeat: -1
      })
    })
    // this.charBody.setOffset(config.body?.offsetX || 0, config.body?.offsetY || 0)

    // this.add(this.voxbox)
    // const nameLabel = new NameLabel(
    //   config.level,
    //   this.name,
    //   this.sprite as GameObjectInWorld
    // )
    // this.add(nameLabel)

    // this.sprite.on('animationstart', (anim, frame) => { console.log(`start of ${anim}`) })
    // this.sprite.on('animationrepeat', (anim, frame) => { console.log(`update of ${anim}`) })
    // this.sprite.on('animationupdate', (anim, frame) => {
    //     console.log(`is ${anim}, frame is ${frame}`)
    // })

    // this.on('pointerover', () => {
    //   nameLabel.open()
    // })
    // this.on('pointerout', () => {
    //   nameLabel.close()
    // })

    const mach = createNPCMachine(
      this,
      this.scene.map.tileWidth,
      this.scene.pathfinder,
      [
        { x: this.x, y: this.y },
        { x: this.x, y: this.y + 10 },
        { x: this.x + 100, y: this.y }
      ]
    )
    this.scene.machineRegistry.add(
      this.name,
      interpret(mach, { devTools: true })
    )
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
    this.charBody.allowDrag = false
    this.facing = dir
    this.playAnim(Dir[dir])
    this.charBody.setVelocity(this.groundVel.x || 0, this.groundVel.y || 0) // this is needed again to cancel diagonole shit
    this.charBody.velocity.add(
      new Phaser.Math.Vector2(
        this.speed * VelocityMap[dir][0],
        this.speed * VelocityMap[dir][1]
      )
    )
  }
  dash(dir: Dir): void {
    this.showDashboxes()
    this.charBody.allowDrag = true
    this.charBody.setDrag(this.dashDrag, this.dashDrag)
    this.charBody.setVelocity(0)
    this.charBody.setAcceleration(0, 0)
    this.face(dir)
    this.stopAnim()
    this.charBody.setVelocity(
      this.dashVel * VelocityMap[dir][0] + (this.groundVel.x || 0),
      this.dashVel * VelocityMap[dir][1] + (this.groundVel.y || 0)
    )
    createResidualGraphic(
      this.sprite,
      this.x + this.sprite.x,
      this.y + this.sprite.y
    )
  }

  face(dir: Dir): void {
    const dirAnim = this.sprite.anims.get(Dir[dir])
    if (dirAnim) {
      this.sprite.anims.setCurrentFrame(dirAnim.frames[1])
      this.facing = dir
    }
  }
  stop(face?: Dir): void {
    this.charBody.allowDrag = false
    this.charBody.setVelocity(this.groundVel.x || 0, this.groundVel.y || 0)
    this.stopAnim()
    if (face) {
      this.face(face)
      this.facing = face
    }
  }
  align(): Phaser.Types.Math.Vector2Like {
    if (!this.scene.map) return { x: undefined, y: undefined }
    const x_ =
      Math.floor(this.charBody.center.x / this.scene.map.tileWidth) *
      this.scene.map.tileWidth
    const y_ =
      Math.floor(this.charBody.center.y / this.scene.map.tileHeight) *
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
    const point = this.charBody?.center || this.sprite.getCenter()
    this.setPosition(x + (this.x - point.x), y + (this.y - point.y))
  }
  transportNudge(x: number, y: number): void {
    this.setPosition(this.x + x, this.y + y)
  }

  minusCharge(): void {}
  playAnim(anim: string): void {
    if (this.sprite.anims) {
      this.sprite.anims.play(
        { key: anim, delay: 0, repeat: -1, frameRate: 11, startFrame: 1 },
        false
      )
    }
  }
  stopAnim(): void {
    if (this.sprite.anims) {
      this.sprite.anims.stop()
      const dirAnim = this.sprite.anims.get(Dir[this.facing])
      this.sprite.anims.setCurrentFrame(dirAnim.frames[1])
    }
  }

  // UI control
  showDashboxes(): void {}
  minusDashbox(): void {}
  showLabel(): void {}
  hideLabel(): void {}

  // async speak(msg: string, speed?: number): Promise<void> {
  //   await Promise.all([
  //     this.voxbox.speak(msg)
  //     // TODO when we get speaking back
  //     // speakString(msg, this, (config: ITalkingPlayConfig): void => Phaser.Utils.NOOP())
  //   ])
  //   return

  //   // console.log(msg)
  // }

  // private getCenter() {
  //   if (this.charBody) return this.charBody.center
  //   else return this.sprite.getCenter()
  // }

  //move machine
  // sendMsgToMoveMachine(msg: string): void { }
  // startMoveMachine(): void { }
  // pauseMoveMachine(): void { }
  // killMoveMachine(): void { }
}
