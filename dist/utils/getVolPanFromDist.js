export function getVolAndPanFromDistance(playerX, playerY, charX, charY, cameraWidth) {
    const difference = charX - playerX;
    // const distance = Phaser.Math.Distance.BetweenPoints(difference ^ 2 + (charY - playerY) ^ 2)
    const distance = Phaser.Math.Distance.BetweenPoints({ x: playerX, y: playerY }, { x: charX, y: charY });
    const pan = Phaser.Math.Clamp(difference / (cameraWidth / 2), -1, 1);
    const vol = Phaser.Math.Clamp((-(1 / (cameraWidth / 2)) * distance) + 1, 0, 1);
    return [vol, pan];
}
//# sourceMappingURL=getVolPanFromDist.js.map