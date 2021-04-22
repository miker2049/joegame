import 'phaser';
import { ITalkingPlayConfig } from '../sound/synths/Talking';
export default function (str: string, char: {
    x?: number;
    y?: number;
    name?: string;
    scene: Phaser.Scene;
}, speakFunc: (config: ITalkingPlayConfig) => void, speed?: number): Promise<void>;
