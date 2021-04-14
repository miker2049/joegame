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
    owner: 'noowner' | Phaser.GameObjects.GameObject;
    x: number;
    y: number;
}
export default class TextWindow extends Phaser.Scene implements ITextBox {
    phaserDom: Phaser.GameObjects.DOMElement;
    textBuff: string;
    owner: 'noowner';
    init(data: TextWindowData): void;
    open(): void;
    close(): void;
    setMDText(text: string): void;
    appendMDText(text: string): void;
    appendNewLineMDText(text: string): void;
    private updateHTML;
}
