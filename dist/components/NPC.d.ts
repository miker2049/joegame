import 'phaser';
import { Character, CharacterConfig } from './Character';
import { Interpreter } from 'xstate';
import { Dir } from './joegameData';
import SpeechBox from './SpeechBox';
export default class NPC extends Character {
    machine: Interpreter<any>;
    interests: Array<{
        x: number;
        y: number;
        finalFacing?: Dir;
    }>;
    currInterest: number;
    patience: number;
    name: string;
    speechbox: SpeechBox;
    auto: boolean;
    constructor(scene: Phaser.Scene, x: number | undefined, y: number | undefined, config: CharacterConfig, interests: Array<{
        x: number;
        y: number;
        finalFacing?: Dir;
    } | Phaser.Tilemaps.Tile>);
    idlingAction(): void;
    initColliders(): void;
}
//# sourceMappingURL=NPC.d.ts.map