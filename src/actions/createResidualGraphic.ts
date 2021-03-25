import 'phaser'

export default function(char: Phaser.GameObjects.Image, x_?: number, y_?: number) {
    let residual = char.scene.add.image( x_ || char.x, y_ || char.y, char.texture.key, char.frame.name)
        .setScale(char.scale)
        .setAlpha(0.5)
        .setDepth(10);

    char.scene.tweens.add({
        targets: [residual],
        alpha: 0,
        duration: 500,
        onComplete: ()=>{residual.destroy()}
    })
}
