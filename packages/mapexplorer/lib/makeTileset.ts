/**
 * Make a tileset like pixi likes from an image of regular tiles
 */

import { Assets, Cache, Spritesheet, Texture } from "pixi.js";

export async function makeTileset(
    imgPath: string,
    tw: number,
    th: number,
    margin = 0,
    spacing = 0,
) {
    const texture = Texture.from(imgPath);
    const trows = Math.floor(texture.height / th);
    const tcols = Math.floor(texture.width / tw);
    const frames = [];
    for (let row = 0; row < trows; row++)
        for (let col = 0; col < tcols; col++)
            frames.push([
                `${row * tcols + col}_grass.png`,
                {
                    frame: { x: col * tw, y: row * th, w: tw, h: th },
                    sourceSize: { w: tw, h: th },
                    spriteSouceSize: {
                        x: 0,
                        y: 0,
                        w: tw,
                        h: th,
                    },
                },
            ]);

    const spritesheet = new Spritesheet(texture, {
        frames: Object.fromEntries(frames),
        meta: {
            image: imgPath,
            size: { w: texture.width, h: texture.height },
            scale: 1,
        },
    });
    await spritesheet.parse();
    return spritesheet;
}
