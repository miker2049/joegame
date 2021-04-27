import 'phaser';
import { ITextBox } from '../components/TextWindow';
interface TextWindowConfig {
    game: Phaser.Game;
    x?: number;
    y?: number;
    width?: number;
    height?: number;
    text?: string;
    additionalStyle?: string;
}
export default function (config: TextWindowConfig): ITextBox;
export {};
