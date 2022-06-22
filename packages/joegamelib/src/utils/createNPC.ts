import { StateMachine } from 'xstate'
import { createNPCMachine } from '../components/NPCMachine'
import createCharacter from '../factories/createCharacter'
import { ICharacter } from '../ICharacter'
import { ILevelComponents } from '../ILevel'
import { InterestSet } from '../joegameTypes'

export default function(name: string, interestSets: InterestSet, level: ILevelComponents, depth: number): [ICharacter, StateMachine<any, any, any>] {
    const char = createCharacter(name, interestSets[0].x, interestSets[0].y, level, depth)
    const mach = createNPCMachine(char, level.map.tileWidth, level.pathfinder, interestSets)
    return [char, mach]
}
