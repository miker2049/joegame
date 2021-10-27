import { ILevelComponents } from '../ILevel'
import touchToDir from '../utils/touchToDir'


export default function(level: ILevelComponents): void {
    level.scene.physics.world.colliders.removeAll()
    level.scene.physics.world.addCollider(level.npcs, level.npcs, (npc1, npc2) => {
        level.machineRegistry.sendTo(npc1.name + '_machine', { type: 'BUMP', sprite: npc2 })
        level.machineRegistry.sendTo(npc2.name + '_machine', { type: 'BUMP', sprite: npc1 })
    })
    level.scene.physics.world.addCollider(level.player, level.npcs, (npc1, npc2) => {
        level.machineRegistry.sendTo('player_machine', { type: 'BUMP', dir: touchToDir(npc2.body.touching) })
        level.machineRegistry.sendTo(npc2.name + '_machine', { type: 'BUMP', sprite: npc1 })
    })

    level.map.layers.forEach((l) => {
        level.scene.physics.world.addCollider(l.tilemapLayer, level.npcs, (npc1, npc2) => {
            level.machineRegistry.sendTo(npc1.name + '_machine', { type: 'BUMP', sprite: npc2 })
            level.machineRegistry.sendTo(npc2.name + '_machine', { type: 'BUMP', sprite: npc1 })
        })
        level.scene.physics.world.addCollider(l.tilemapLayer, level.player, (npc1, npc2) => {
            level.machineRegistry.sendTo(npc1.name + '_machine', { type: 'BUMP', sprite: npc2 })
            level.machineRegistry.sendTo('player_machine', { type: 'BUMP', sprite: npc1 })
        })

    })
    level.scene.physics.world.setBounds(0,0,level.map.widthInPixels,level.map.heightInPixels)
    level.player.charBody.setCollideWorldBounds(true)

}
