import 'phaser'

export default function(go: Phaser.Types.Math.Vector2Like, tilesize: number): Phaser.Types.Math.Vector2Like {
        const x_ = (Math.floor((go.x)/tilesize) * tilesize)
        const y_ = (Math.floor((go.y)/tilesize) * tilesize)
        return {x: x_/tilesize, y: (y_/tilesize)}
}
