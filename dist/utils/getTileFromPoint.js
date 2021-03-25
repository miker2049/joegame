import 'phaser';
export default function (go, tilesize) {
    const x_ = (Math.floor((go.x) / tilesize) * tilesize);
    const y_ = (Math.floor((go.y) / tilesize) * tilesize);
    return { x: x_ / tilesize, y: (y_ / tilesize) };
}
//# sourceMappingURL=getTileFromPoint.js.map