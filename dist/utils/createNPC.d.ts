import { StateMachine } from 'xstate';
import { ICharacter } from '../ICharacter';
import { ILevelComponents } from '../ILevel';
import { InterestSet } from '../joegameTypes';
export default function (name: string, interestSets: InterestSet, level: ILevelComponents): [ICharacter, StateMachine<any, any, any>];
