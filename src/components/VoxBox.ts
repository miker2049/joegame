import 'phaser'
import { typewriteText } from '../utils/typewriteText';
import { ILevelComponents } from '../ILevel'
import { ITextBox } from './TextWindow'

const BOXALPHA = 0.7
export default class VoxBox extends Phaser.GameObjects.Text implements ITextBox {
    textbuff: string
    owner: 'noowner' | Phaser.GameObjects.GameObject = 'noowner'
    closeEvent: { destroy(): void } | undefined

    constructor(level: ILevelComponents, owner?: 'noowner' | Phaser.GameObjects.GameObject) {
        super(level.scene, 0, 4, '', {
            // fontFamily: 'Retro Gaming',
            fontSize: '12px',
            wordWrap: {
                width: level.map.tileWidth * 7,

            },
            padding: { x: 2, y: 2 },
            fixedWidth: level.map.tileWidth * 7,
            fixedHeight: level.map.tileWidth * 3.5,
        });
        if (owner) this.owner = owner
        this.setWordWrapCallback((str) => {
            const wrapped = this.basicWordWrap(str, this.context, level.map.tileWidth * 7)
            let splitt = wrapped.split('\n')
            return splitt.slice(-4)
        })
        this.textbuff = ''
        this.setAlpha(BOXALPHA)
        this.setBackgroundColor('black')
        this.setOrigin(0.5, 1)
        this.setScale(1 / (level.scene.cameras.default.zoom * 2))
        // this.setMaxLines
    }

    async speak(str: string, speed?: number) {
        if (this.closeEvent) this.closeEvent.destroy()
        this.setMDText('')
        await this.open()
        const timeID = Math.random().toString()
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
        return new Promise<void>((res, rej) => {
            this.scene.tweens.add({
                targets: [this],
                alpha: BOXALPHA,
                duration: 500,
                onComplete: () => res()
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
        this.textbuff += "\n\n" + text
        this.updateVoxtext()
    }

    updateVoxtext() {
        this.setText(this.textbuff)
        this.updateText()
    }
}
