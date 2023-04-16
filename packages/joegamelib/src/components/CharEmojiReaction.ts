import defaults from '../defaults'
import emojiMap from '../utils/emojiMap'
import loadAfterLoad from '../utils/loadAfterLoad'
import { ICharacter } from '../ICharacter'

export class CharEmojiReaction extends Phaser.GameObjects.Image {
  constructor(scene: Phaser.Scene, emoji: string) {
    super(scene, 0, 0, emoji, 0)
    this.setScale(0.125)
  }
}

export async function getEmojiObject(emoji: string, scene: Phaser.Scene) {
  if (!scene.textures.exists(emoji)) {
    await loadAfterLoad(
      scene,
      emoji,
      defaults.emojiPath +
        (emojiMap.get(emoji) ?? emojiMap.get(defaults.emoji)),
      'image'
    )
  }
  return new CharEmojiReaction(scene, emoji)
}

export async function happyEmojiReact(char: ICharacter) {
  const emoji = await getEmojiObject('happy', char.scene)
  const scene = char.scene
  emoji.setScale(0.125)
  emoji.setY(10)
  char.add(emoji)
  scene.tweens.add({
    targets: emoji,
    y: '-=5',
    ease: 'Bounce',
    repeat: 0,
    duration: 1500,
    onComplete: () => {
      char.remove(emoji)
      emoji.destroy()
    }
  })
}

const randomDev = (dev: number) => Math.random() * dev - dev / 2

export async function sparkleCircle(char: ICharacter) {
  const emoji = await getEmojiObject('sparkle', char.scene)
  const scene = char.scene
  emoji.setScale(0.125)
  emoji.setY(12)
  emoji.setX(26)
  char.add(emoji)
  const circle = new Phaser.Curves.Ellipse(
    randomDev(2),
    randomDev(2),
    10 + randomDev(5),
    10 + randomDev(5)
  )
  const fromm = Math.random() > 0.5 ? 0 : 1
  const too = fromm === 1 ? 0 : 1

  scene.tweens.add({
    targets: [emoji],
    alpha: {
      from: 0,
      to: 1
    },
    duration: 500
  })
  sweatReaction(char)
  scene.tweens.addCounter({
    ease: 'linear',
    from: fromm,
    to: too,
    repeat: 4,
    duration: 1500,
    onUpdate: (tween) => {
      const point = circle.getPoint(tween.getValue())
      emoji.setPosition(point.x, point.y)
    },
    onComplete: () => {
      scene.tweens.add({
        targets: [emoji],
        alpha: {
          from: 1,
          to: 0
        },
        duration: 500,
        onComplete: () => {
          emoji.destroy()
        }
      })
    }
  })
}
// export async function angelsFlyingAroundHead()

export async function sweatReaction(char: ICharacter) {
  await getEmojiObject('sweat', char.scene)
  const part = char.scene.add.particles('sweat')
  const scene = char.scene
  part.setY(12)
  part.setX(16)
  let emitConfig = defaults.emitterConfig
  const circle = new Phaser.Curves.Ellipse(
    randomDev(2),
    randomDev(2),
    10 + randomDev(5),
    10 + randomDev(5)
  )
  const fromm = Math.random() > 0.5 ? 0 : 1
  const too = fromm === 1 ? 0 : 1

  Object.assign(emitConfig, {
    scale: 0.124,
    speedX: { min: -10, max: 10 },
    speedY: { min: -10, max: 10 },
    emitZone: {
      type: 'random',
      source: circle
    }
  })
  console.log(char.sprite.depth - 2)
  const emit = part.createEmitter(emitConfig)
  part.setDepth(char.sprite.depth - 2)

  char.add(part)
  scene.time.addEvent({ delay: 2000, callback: () => emit.remove() })
  scene.tweens.add({
    targets: [part],
    alpha: {
      from: 0,
      to: 1
    },
    duration: 500
  })
}
