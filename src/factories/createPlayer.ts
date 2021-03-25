import { interpret, Machine } from 'xstate'
import createCharacter from './createCharacter'
import { createMoveMachine } from '../components/MoveMachine'
import { ILevelComponents } from '../ILevel'
import MoveController from '../MoveController'
import { ICharacter } from '../ICharacter'

export default function(name: string, x: number, y: number, level: ILevelComponents): ICharacter {
    const char = createCharacter(name,x,y,level)
    const moveMachine = interpret(createMoveMachine(char, level.map.tileWidth, level.pathfinder), {devTools: true, parent: interpret(Machine({id:name+'dummyparent'}))})

    level.machineRegisty.add('player_machine',moveMachine)
    new MoveController(moveMachine, level.scene)
    return char
}
