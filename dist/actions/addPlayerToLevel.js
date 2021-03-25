import 'phaser';
import createPlayer from '../factories/createPlayer';
export default function (level, x, y, char) {
    const player = createPlayer(char ? char : "default", x, y, level);
    level.scene.add.existing(player);
    level.player = player;
    return player;
}
//# sourceMappingURL=addPlayerToLevel.js.map