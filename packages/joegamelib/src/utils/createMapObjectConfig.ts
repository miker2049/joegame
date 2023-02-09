import { ILevelComponents } from '../ILevel'
import d from '../defaults'
import { IMapObjectConfig, ITiledMapObject } from '../components/MapObject'
import { LevelScene } from '../LevelScene'

export function createMapObjectConfig(
  obj: ITiledMapObject,
  scene: LevelScene
): IMapObjectConfig {
  let props: Record<string, any> = {}
  if (obj.properties) {
    for (let prop of obj.properties) {
      props[prop.name] = prop.value
    }
  }

  let texture: string = d.texture
  if (props.req_image) {
    texture = props.req_image.split(',')[0]
  }
  let frame: number = 0

  const x = obj.x || 0
  const y = obj.y || 0
  return {
    x,
    y,
    name: obj.name || `${x}+${y}`,
    id: obj.id,
    texture,
    frame,
    flipX: obj.flippedHorizontal || false,
    flipY: obj.flippedVertical || false,
    scrollFactor: props.scrollFactor || 1,
    visible: obj.visible || true,
    depth: props.depth || obj.depth,
    rotation: obj.rotation || 0,
    originX: props.originX || 0,
    originY: props.originY || 1,
    width: obj.width || 16,
    height: obj.height || 16,
    body: props.body || false,
    moveable: props.moveable || false,
    tint: props.tint
      ? Phaser.Display.Color.HexStringToColor(props.tint.substring(3, 9)).color
      : -1,
    popupText: props.popupText ?? '',
    scene
  }
}

function getTextureFromGID(
  gid: number,
  level: ILevelComponents
): [string, number] | undefined {
  if (level.scene.textures.exists(gid.toString())) {
    return [gid.toString(), 0]
  } else {
    //if there is a gid but not a texture itself, its in one of the tilesheets/spritemaps
    const found = level.map.tilesets.find((tset, ind, tsets) => {
      // is the gid in question equal to or over this sets first gid? Ok, is it beneath the next one, or already on the last one?

      return gid >= tset.firstgid && tsets[ind + 1]
        ? gid < tsets[ind + 1]?.firstgid
        : true
    })
    if (found) {
      return [found.name, gid - found.firstgid]
    }
  }
}
