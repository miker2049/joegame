import 'phaser';
import { ILevelComponents } from '../ILevel';
import { ITextBox } from './TextWindow';
export default class VoxBox extends Phaser.GameObjects.Text implements ITextBox {
    textbuff: string;
    owner: 'noowner' | Phaser.GameObjects.GameObject;
    constructor(level: ILevelComponents, owner?: 'noowner' | Phaser.GameObjects.GameObject);
    speak(str: string, speed?: number): Promise<void>;
    open(): void;
    close(): void;
    setMDText(text: string): void;
    appendMDText(text: string): void;
    appendNewLineMDText(text: string): void;
    updateVoxtext(): void;
}
