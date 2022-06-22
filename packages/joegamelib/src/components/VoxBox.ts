import { GameObjectInWorld } from 'joegameTypes';
import 'phaser';
import { ILevelComponents } from '../ILevel';
import { typewriteText } from '../utils/typewriteText';
import TextBox from './TextBox';

const BOXALPHA = 0.7
const TILEWIDTH = 16

export default class VoxBox extends TextBox {

    constructor(level: ILevelComponents, owner?: GameObjectInWorld) {
        super({
            fontSize: 8,
            width: TILEWIDTH * 7,
            height: TILEWIDTH * 3.5,
            alpha: 0.7,
            color: 'black',
            fontColor: 'white',
            x: 0,
            y: 4,
            text: '',
            originX: 0.5,
            originY: 1,
            paddingX: 2,
            paddingY: 2,
            lineN: 4,
            scale: 1 / (level.scene.cameras.default.zoom * 2),
            level,
            owner,
        })
    }
}
