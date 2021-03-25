import Character from "../Character";
import { ICharacter } from "../ICharacter";

export default function(gobject: Phaser.GameObjects.GameObject | ICharacter) {
    gobject.scene.tweens.add({
        targets: [gobject],
        onStart: () => {
            // this.charBody.setVelocity(0,0);
            if (gobject instanceof Character) {
                gobject.charBody.setEnable(false);
            }
        },
        onComplete: () => {
            if (gobject instanceof Character) {
                gobject.charBody.setEnable(true);
            }
        },
        y: '-= 4',
        ease: 'Quad',
        duration: 100,
        yoyo: true,
    });
}
