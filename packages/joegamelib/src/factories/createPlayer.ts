import { interpret, Machine } from 'xstate'
import createCharacter from './createCharacter'
import { createMoveMachine } from '../components/MoveMachine'
import { ILevelComponents } from '../ILevel'
import MoveController from '../MoveController'
import { ICharacter } from '../ICharacter'

export default function(name: string, x: number, y: number, level: ILevelComponents, depth: number = 7): ICharacter {
    const char = createCharacter(name, x, y, level, depth)
    const moveMachine = interpret(createMoveMachine(char, level.map.tileWidth, level.pathfinder), { devTools: true, parent: interpret(Machine({ id: name + 'dummyparent' })) })

    level.machineRegistry.add('player_machine', moveMachine)
    char.sprite.on('animationrepeat', () => {
        level.toner.play({ inst: 'walk' })
    })
    new MoveController(moveMachine, level.scene)
    return char
}
