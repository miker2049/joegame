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
    owner: 'noowner' | Phaser.GameObjects.GameObject
    x: number
    y: number
    width: number
    height: number
}

export class TextWindow extends Phaser.Scene implements ITextBox {
    phaserDom!: Phaser.GameObjects.DOMElement
    textBuff: string = ''
    owner: 'noowner' = 'noowner'

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

    get width(){
        return this.phaserDom.width
    }

    get height(){
        return this.phaserDom.height
    }

    get x(){
        return this.phaserDom.x
    }

    get y(){
        return this.phaserDom.y
    }

    set x(x: number){
        this.phaserDom.x = x
    }

    set y(y:number){
        this.phaserDom.y = y
    }

    set style(str: string){
        let html = this.phaserDom.node.innerHTML
        this.phaserDom.createElement('div', str, html)
        this.phaserDom.setHTML(html)
    }

    set html(str: string){
        this.phaserDom.setHTML(str)
    }
    get html(): string {
        return this.phaserDom.node.innerHTML
    }

    setDomWidth(w: string | number) {
        this.appendStyle('width',
                        typeof w == 'string' ? w : `${w}px` )
    }

    setDomHeight(w: string | number) {
        this.appendStyle('height',
                        typeof w == 'string' ? w : `${w}px` )
    }

    setDomSize(w: string | number, h: string | number) {
       this.setDomWidth(w)
       this.setDomHeight(h)
    }

    appendStyle(prop: string, v: string) {
        this.width
        // @ts-ignore
        this.phaserDom.node.style[prop] = v

    }

    private updateHTML(): void { this.phaserDom.setHTML(mdParse(this.textBuff)) }
}
