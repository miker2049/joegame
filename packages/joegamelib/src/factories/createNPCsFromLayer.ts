import 'phaser'
import { ICharacter } from '../ICharacter'
import { ILevelComponents } from '../ILevel'
import { NPCContext, NPCEvent } from '../components/NPCMachine'
import { StateMachine } from 'xstate'
import createNPC from '../utils/createNPC'

export default function* (
  layer: string,
  level: ILevelComponents,
  depth: number
): Iterable<[ICharacter, StateMachine<NPCContext, any, NPCEvent>]> {
  if (!level.map.getObjectLayer(layer)) {
    return
  }
  // first, create an object with keys that correspond to the npc names
  // that have values which are sets of locations in absolute pixels
  let interestSets: any = {}
  for (let obj_ of level.map.getObjectLayer(layer).objects) {
    if (interestSets[obj_.name]) {
      interestSets[obj_.name].push({ x: obj_.x, y: obj_.y, NPCtype: obj_.type })
    } else {
      interestSets[obj_.name] = [{ x: obj_.x, y: obj_.y, NPCtype: obj_.type }]
    }
  }
  // now we iterate through our new object and create the npcs
  for (let name in interestSets) {
    // create the npc
    yield createNPC(name, interestSets[name], level, depth)
  }
}
