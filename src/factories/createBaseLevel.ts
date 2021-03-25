import 'phaser'
import {ILevelComponents} from '../ILevel'
import { Level } from '../Level'
export default function(game: Phaser.Game, mapjsonpath: string): ILevelComponents {return new Level(game, mapjsonpath)}
