import 'phaser'
import mdParse from '../utils/mdParse'

export interface TextWindowData {
    x: number
    y: number
    height?: number
    width?: number
    text?: string
    additionalStyle?: string
}

export interface ITextBox {
    open(): void
    close(): void
    setMDText(text: string): void
    appendMDText(text: string): void
    appendNewLineMDText(text: string): void
}

export default class TextWindow extends Phaser.Scene implements ITextBox {
    phaserDom!: Phaser.GameObjects.DOMElement
    textBuff: string = ''

    init(data: TextWindowData) {
        const style = ` list-style: none;
             background-color: rgba(0,0,0,0.8);
             overflow: hidden;
             width:${data.width ?? 250}px;
             margin: 0px;
             padding: 1em;
             color: white;
             font-family: Retro Gaming;
             height:${data.height ?? 250}px;
             -ms-overflow-style:none;
             scrollbar-width: none;`
        this.phaserDom = this.add.dom(data.x, data.y, 'div', style + data.additionalStyle ?? '').setOrigin(0, 0)
        if (data.text) {
            this.setMDText(data.text)
        }

        this.input.keyboard.enabled = true;
        this.phaserDom.alpha = 0;

        this.input.keyboard.on('keyup', (event: KeyboardEvent) => {
            if (event.key === 'Enter' || event.key === 'Escape') {
                this.close();
            }
        })
    }

    open() {
        this.tweens.add({
            targets: [this.phaserDom],
            alpha: 1,
            duration: 500
        })
    }
    close() {
        this.input.keyboard.enabled = false;

        this.tweens.add({
            targets: [this.phaserDom],
            alpha: 0,
            duration: 500
        })
    }
    setMDText(text: string) {
        this.textBuff = text
        this.updateHTML()
    }
    appendMDText(text: string) {
        this.textBuff += text
        this.updateHTML()
    }
    appendNewLineMDText(text: string) {
        this.textBuff += "\n\n" + text
        this.updateHTML()
    }
    private updateHTML(): void { this.phaserDom.setHTML(mdParse(this.textBuff)) }
}
