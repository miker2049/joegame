import 'phaser';
export default function (go, tilesize) {
    if (go.x === undefined || go.y === undefined)
        return;
    const x_ = (Math.floor((go.x) / tilesize) * tilesize);
    const y_ = (Math.floor((go.y) / tilesize) * tilesize);
    return { x: x_ / tilesize, y: (y_ / tilesize) };
}
//# sourceMappingURL=getTileFromPoint.js.map