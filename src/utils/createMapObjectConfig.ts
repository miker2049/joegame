import { ILevelComponents } from 'ILevel'
import d from '../defaults'
import { IMapObjectConfig } from '../components/MapObject'

export default function(obj: Phaser.Types.Tilemaps.TiledObject, level: ILevelComponents): IMapObjectConfig {

    let props: any
    if (obj.properties) {
        for (let prop of obj.properties) {
            props[prop.name] = prop.value
        }
    }



    let texture: string = d.texture
    let frame: number = 0

    if (obj.gid) {
        const gidFound = getTextureFromGID(obj.gid, level)
        if (gidFound) {
            texture = gidFound[0]
            frame = gidFound[1]
        }
    } else if (props.texture) {
        texture = props.texture
    } else {
        const wikiobj = level.scene.game.cache.json.get('gdata').mapobject.get(obj.type)
        if (wikiobj) {
            texture = wikiobj.req_spritesheet[0]
        }
    }
    const x = obj.x || 0
    const y = obj.y || 0
    return {
        x,
        y,
        name: obj.name || `${x}+${y}`,
        id: obj.id,
        tiledWidth: obj.width || 2,
        tiledHeight: obj.height || 2,
        typee: obj.type,
        texture,
        frame,
        flipX: obj.flippedHorizontal || false,
        flipY: obj.flippedVertical || false,
        scrollFactor: props.scrollFactor || 0,
        visible: obj.visible || true,
        depth: props.depth || d.objectDepth,
        rotation: obj.rotation || 0,
        originX: props.originX || 0,
        originY: props.originY || 0,
        width: obj.width || 16,
        height: obj.height || 16,
        body: props.body || false,
        moveable: props.moveable || false,
        tint: props.tint ? Phaser.Display.Color.HexStringToColor(props.substring(3, 9)).color : 0,
        popupText: props.popupText ?? ''
    }
}

function getTextureFromGID(gid: number, level: ILevelComponents): [string, number] | undefined {

    if (level.scene.textures.exists(gid.toString())) {
        return [gid.toString(), 0]
    } else {
        //if there is a gid but not a texture itself, its in one of the tilesheets/spritemaps
        const found = level.map.tilesets.find((tset, ind, tsets) => {
            // is the gid in question equal to or over this sets first gid? Ok, is it beneath the next one, or already on the last one?

            return gid >= tset.firstgid && tsets[ind + 1] ? gid < tsets[ind + 1]?.firstgid : true
        });
        if (found) {
            return [found.name, gid - found.firstgid]
        }
    }
}
