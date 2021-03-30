import 'phaser';
export interface TextWindowData {
    x: number;
    y: number;
    height?: number;
    width?: number;
    text?: string;
    additionalStyle?: string;
}
export interface ITextBox {
    open(): void;
    close(): void;
    setMDText(text: string): void;
    appendMDText(text: string): void;
    appendNewLineMDText(text: string): void;
}
export default class TextWindow extends Phaser.Scene implements ITextBox {
    phaserDom: Phaser.GameObjects.DOMElement;
    textBuff: string;
    init(data: TextWindowData): void;
    open(): void;
    close(): void;
    setMDText(text: string): void;
    appendMDText(text: string): void;
    appendNewLineMDText(text: string): void;
    private updateHTML;
}
//# sourceMappingURL=TextWindow.d.ts.map