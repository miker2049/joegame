import { ILevelComponents } from 'ILevel';
import TiledRawJSON from 'types/TiledRawJson';
import getDepthMap from 'utils/getDepthMap';
import { getMapKeyName, getMapKeyNameRaw } from '../utils/getKeyNames';

export default function(level: ILevelComponents, mapjsonpath: string, offsetX?: number, offsetY?: number): Phaser.Tilemaps.Tilemap {
    const scene = level.scene
    const tilemap = scene.make.tilemap({ key: getMapKeyName(mapjsonpath) })
    // const depthmap = scene.game.registry.get('depthmap')
    //initialize tilesets, and also leave a reference to them so they can easily be used in making the layers
    for (let tileset of tilemap.tilesets) {
        // Note that here, to keep things simpler, every tileset is preloaded with the name of the filename itself, so two `tileset.name`s
        if (tileset.total > 1) {
            tilemap.addTilesetImage(tileset.name, tileset.name, tileset.tileWidth, tileset.tileHeight, tileset.tileMargin, tileset.tileSpacing);
        }
    }
    const depthmap = getDepthMap(scene.game, mapjsonpath)
    // init all our layers...
    tilemap.layers.forEach((l, i) => {
        const lay = tilemap.createLayer(l.name, tilemap.tilesets, offsetX || 0, offsetY || 0)
        lay.setDepth(depthmap.get(l.name) ?? 0)

        //do not make visible layers that begin with underscore
        if (l.name[0] == '_') {
            lay.setVisible(false)
        }

        l.tilemapLayer.setCollisionByProperty({ collides: true })

        if (level.config.lights) {
            l.tilemapLayer.setPipeline('Light2D')
        }
    })
    tilemap.createBlankLayer('highlight', tilemap.tilesets).setVisible(true)


    //tiled defined animated tiles
    const rawtiled: TiledRawJSON = scene.game.cache.json.get(getMapKeyNameRaw(mapjsonpath))
    if (rawtiled) {
        rawtiled.tilesets.forEach(tileset => {
            if (tileset.tiles) {
                tileset.tiles
                    .filter((tile, index) => {
                        if (tile.animation) {
                            return true
                        } else {
                            return false
                        }
                    })
                    .forEach((tile) => {
                        tilemap.layers.forEach(layer => {
                            layer.tilemapLayer.createFromTiles(
                                tile.id + tileset.firstgid,
                                // cast here because Phaser should be accepting null for non-replacement
                                null as unknown as number,
                                {
                                    key: tileset.name,
                                    frame: tile.id,
                                    add: true,
                                    origin: {
                                        y: 0, x: 0
                                    }
                                }
                            ).forEach(spr => {
                                const animFramesId = tile.animation!.map(v => v.tileid)
                                const animFrames = scene.anims.generateFrameNumbers(
                                    tileset.name,
                                    {
                                        start: animFramesId[0],
                                        end: animFramesId[animFramesId.length - 1],
                                    }
                                )
                                spr.anims.create({
                                    frames: animFrames,
                                    key: 'local_tile_anim',
                                    repeat: -1,
                                    frameRate: 3
                                })
                                spr.anims.play({ key: 'local_tile_anim', showOnStart: true })
                                spr.setDepth(layer.tilemapLayer.depth + 1)
                            })
                        })
                    })
            }
        })
    }
    return tilemap
}
