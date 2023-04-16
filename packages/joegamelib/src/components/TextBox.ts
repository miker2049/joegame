import { GameObjectInWorld } from '../joegameTypes'
import { typewriteText } from '../utils/typewriteText'
import { ITextBox } from './TextWindow'
import { LevelScene } from '../LevelScene'

interface ITextBoxConfig {
  fontSize: number
  width: number
  height: number
  alpha: number
  color: string
  fontColor: string
  x: number
  y: number
  originX: number
  originY: number
  paddingX: number
  paddingY: number
  scale: number
  level: LevelScene
  owner?: GameObjectInWorld
  lineN: number
  text: string
}

export default class TextBox
  extends Phaser.GameObjects.Text
  implements ITextBox
{
  textbuff: string
  baseAlpha: number
  owner?: GameObjectInWorld

  closeEvent: { destroy(): void } | undefined

  constructor({
    level,
    fontSize,
    width,
    height,
    alpha,
    color,
    fontColor,
    x,
    y,
    paddingX,
    paddingY,
    originX,
    originY,
    lineN,
    owner,
    scale,
    text
  }: ITextBoxConfig) {
    super(level, x, y, '', {
      fontFamily: 'Retro Gaming',
      fontSize: `${fontSize}px`,
      color: fontColor,
      wordWrap: {
        width: width
      },
      padding: { x: paddingX, y: paddingY },
      fixedWidth: width,
      fixedHeight: height
    })
    if (owner) this.owner = owner
    this.setDepth(8000)
    this.setWordWrapCallback((str) => {
      const wrapped = this.basicWordWrap(str, this.context, width)
      let splitt = wrapped.split('\n')
      return splitt.slice(-lineN)
    }, this)
    this.textbuff = text
    this.updateVoxtext()

    this.baseAlpha = alpha
    this.setAlpha(this.baseAlpha)
    this.setBackgroundColor(color)
    this.setOrigin(originX, originY)
    this.setScale(scale)
    this.close()
    // this.setMaxLines
  }

  async speak(str: string, speed?: number) {
    if (this.closeEvent) this.closeEvent.destroy()
    this.setMDText('')
    await this.open()
    await typewriteText(str, this, this.scene, speed)
    // this.setText(str)
    this.closeEvent = this.scene.time.addEvent({
      delay: 1500,
      callback: () => {
        this.close()
      }
    })
  }

  open() {
    return new Promise<void>((res) => {
      this.scene.tweens.add({
        targets: [this],
        alpha: this.baseAlpha,
        duration: 500,
        onComplete: res
      })
    })
  }

  close() {
    this.scene.tweens.add({
      targets: [this],
      alpha: 0,
      duration: 500
    })
  }

  //TODO stop lying about this
  setMDText(text: string) {
    this.textbuff = text
    this.updateVoxtext()
  }

  appendMDText(text: string) {
    this.textbuff += text
    this.updateVoxtext()
  }

  appendNewLineMDText(text: string) {
    this.textbuff += '\n\n' + text
    this.updateVoxtext()
  }

  updateVoxtext() {
    this.setText(this.textbuff)
    this.updateText()
  }
}
