import 'phaser';
import { ICharacter } from '../ICharacter';
import { ILevelComponents } from '../ILevel';
import { NPCContext, NPCEvent } from '../components/NPCMachine';
import { StateMachine } from 'xstate';
export default function (layer: string, level: ILevelComponents, depth?: number): Iterable<[ICharacter, StateMachine<NPCContext, any, NPCEvent>]>;
