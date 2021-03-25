import 'phaser'

export default function(go: Phaser.Types.Physics.Arcade.GameObjectWithBody, tilesize: number): Phaser.Types.Math.Vector2Like {
    if (go.body) {
        const x_ = (Math.floor((go.body.center.x) / tilesize) * tilesize)
        const y_ = (Math.floor((go.body.center.y) / tilesize) * tilesize)
        return { x: x_ / tilesize, y: (y_ / tilesize) + 1 }
    } else {
        return { x: 0, y: 0 }
    }
}
