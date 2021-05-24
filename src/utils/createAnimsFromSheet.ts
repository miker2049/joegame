import 'phaser'
export default function createAnimsFromSheet(sheetKey: string, animLength: number, game: Phaser.Game): void {

    const sheet: Phaser.Textures.Texture = game.textures.get(sheetKey);
    // debugger;
    for (let i = 0; i < sheet.frameTotal - 1; i = i + animLength) {
        // console.log(game.anims.generateFrameNumbers(sheetKey, { start: i, end: i + (animLength - 1), }))
        game.anims.create({
            key: `${sheetKey}_anim_${i}`,
            frames: game.anims.generateFrameNumbers(sheetKey, { start: i, end: i + (animLength - 1) }),
            frameRate: 10,
            repeat: -1

        })

    }
}
