import 'phaser';
export default function (map: Phaser.Tilemaps.Tilemap, amount?: number, start?: {
    x: number;
    y: number;
}, bounds?: {
    x: number;
    y: number;
    width: number;
    height: number;
}): {
    x: any;
    y: any;
}[];
