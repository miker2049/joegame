import 'phaser';
export default function createAnimsFromSheet(sheetKey, animLength, game) {
    const sheet = game.textures.get(sheetKey);
    for (let i = 0; i < sheet.frameTotal - 1; i = i + animLength) {
        // console.log(game.anims.generateFrameNumbers(sheetKey, { start: i, end: i + (animLength - 1), }))
        game.anims.create({
            key: `${sheetKey}_anim_${i}`,
            frames: game.anims.generateFrameNumbers(sheetKey, { start: i, end: i + (animLength - 1) }),
            frameRate: 10,
            repeat: -1
        });
    }
}
//# sourceMappingURL=createAnimsFromSheet.js.map