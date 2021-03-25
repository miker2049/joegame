import touchToDir from '../utils/touchToDir';
export default function (level) {
    level.scene.physics.world.addCollider(level.npcs, level.npcs, (npc1, npc2) => {
        level.machineRegisty.sendTo(npc1.name + '_machine', { type: 'BUMP', sprite: npc2 });
        level.machineRegisty.sendTo(npc2.name + '_machine', { type: 'BUMP', sprite: npc1 });
    });
    if (level.player) {
        level.scene.physics.world.addCollider(level.player, level.npcs, (npc1, npc2) => {
            level.machineRegisty.sendTo('player_machine', { type: 'BUMP', dir: touchToDir(npc2.body.touching) });
            level.machineRegisty.sendTo(npc2.name + '_machine', { type: 'BUMP', sprite: npc1 });
        });
    }
    level.map.layers.forEach((l) => {
        level.scene.physics.world.addCollider(l.tilemapLayer, level.npcs, (npc1, npc2) => {
            level.machineRegisty.sendTo(npc1.name + '_machine', { type: 'BUMP', sprite: npc2 });
            level.machineRegisty.sendTo(npc2.name + '_machine', { type: 'BUMP', sprite: npc1 });
        });
        if (level.player) {
            level.scene.physics.world.addCollider(l.tilemapLayer, level.player, (npc1, npc2) => {
                level.machineRegisty.sendTo(npc1.name + '_machine', { type: 'BUMP', sprite: npc2 });
                level.machineRegisty.sendTo('player_machine', { type: 'BUMP', sprite: npc1 });
            });
        }
    });
}
//# sourceMappingURL=createLevelPhysics.js.map