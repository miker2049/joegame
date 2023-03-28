import { LevelScene } from '../LevelScene'

export default function (
  scene: LevelScene,
  offsetX?: number,
  offsetY?: number
): Phaser.Tilemaps.Tilemap {
  const tilemap = scene.make.tilemap({ key: 'map' })
  // const depthmap = scene.game.registry.get('depthmap')
  //initialize tilesets, and also leave a reference to them so they can easily be used in making the layers
  tilemap.tilesets
  for (let i in tilemap.tilesets) {
    const tileset = tilemap.tilesets[i]
    tileset.name = tileset.name
      .split('/')
      .reverse()[0]
      .replace(/\.png$/, '')
    // Note that here, to keep things simpler, every tileset is preloaded with the name of the filename itself, so two `tileset.name`s
    if (tileset.total > 1) {
      tilemap.addTilesetImage(
        tileset.name,
        tileset.name,
        tileset.tileWidth,
        tileset.tileHeight,
        tileset.tileMargin,
        tileset.tileSpacing
      )
    }
  }
  // const depthmap = getDepthMap(scene.game, mapjsonpath)
  // // init all our layers...
  const collidingTiles: Phaser.Tilemaps.Tile[] = []
  tilemap.layers.forEach((l) => {
    const lay = tilemap.createLayer(
      l.name,
      tilemap.tilesets,
      offsetX || 0,
      offsetY || 0
    )
    if (!l.visible) lay.setVisible(false)
    // lay.setDepth(depthmap.get(l.name) ?? 0)
    //do not make visible layers that begin with underscore
    if (l.name.match(/stack/)) {
      lay.setDepth(1000)
    }

    l.tilemapLayer.setCollisionByProperty({ collides: true })
    l.tilemapLayer.setCollisionByProperty({ wall: true })
    // l.tilemapLayer.filterTiles((tile) => {})
    if (!l.name.match(/COLLIDERS/)) {
      collidingTiles.concat(
        l.tilemapLayer.filterTiles((tile: Phaser.Tilemaps.Tile) => {
          tile.properties.collides === true || tile.properties.wall === true
        })
      )
    }

    // if (level.config.lights) {
    //   l.tilemapLayer.setPipeline('Light2D')
    // }
  })
  tilemap.setCollision(collidingTiles)
  tilemap.createBlankLayer('highlight', tilemap.tilesets).setVisible(true)

  //tiled defined animated tiles
  createAnimatedTiles(tilemap, scene)

  return tilemap
}

function createAnimatedTiles(
  tilemap: Phaser.Tilemaps.Tilemap,
  scene: LevelScene
) {
  scene.mapjson.tilesets.forEach((tileset) => {
    if (tileset.tiles) {
      tileset.tiles
        .filter((tile) => tile.animation)
        .forEach((tile) => {
          tilemap.layers.forEach((layer) => {
            layer.tilemapLayer
              .createFromTiles(
                tile.id + tileset.firstgid,
                // cast here because Phaser should be accepting null for non-replacement
                null as unknown as number,
                {
                  key: tileset.name,
                  frame: tile.id,
                  add: true,
                  origin: {
                    y: 0,
                    x: 0
                  }
                }
              )
              .forEach((spr, idx) => {
                const animFramesId = tile.animation!.map((v) => v.tileid)
                const animFrames = scene.anims.generateFrameNumbers(
                  tileset.name,
                  {
                    start: animFramesId[0],
                    end: animFramesId[animFramesId.length - 1]
                  }
                )
                console.log(animFrames)
                const key = `local_tile_anim_${
                  tile.id + tileset.firstgid
                }_${idx}`
                spr.anims.create({
                  frames: animFrames,
                  key,
                  repeat: -1,
                  frameRate: 3
                })
                spr.anims.play({ key, showOnStart: true })
                // spr.setDepth(layer.tilemapLayer.depth + 1)
              })
          })
        })
    }
  })
}
