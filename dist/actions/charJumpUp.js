import Character from "../Character";
export default function (gobject) {
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
//# sourceMappingURL=charJumpUp.js.map