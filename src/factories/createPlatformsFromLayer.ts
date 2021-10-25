import Platform from '../components/Platform';
import { IMap, ILevelComponents } from '../ILevel';

export default function*(level: ILevelComponents, layer: string, depth: number): Iterable<Platform>{
    if(!level.map.getObjectLayer(layer)) return;
    let platformSets: any= {}
    for(let obj_ of level.map.getObjectLayer(layer).objects){
        if(platformSets[obj_.name]){
            platformSets[obj_.name].push(obj_)
        } else {
            platformSets[obj_.name] = [obj_];
        }
    }
    // now we iterate through our new object and create the npcs
    for(let plat in platformSets){
        const platform= platformSets[plat]
        let platDur: number=1;
        if (platform[0].properties) {
            platform[0].properties.forEach((prop)=>{
                if (prop.name === 'speed') {
                    platDur = prop.value
                    console.log(platDur)
                }
            })
        }
        const platConfig = {
            level: level,
            x: platform[0].x/level.map.tileWidth,
            y: platform[0].y/level.map.tileHeight,
            width: platform[0].width/level.map.tileWidth,
            height: platform[0].height/level.map.tileHeight,
            endX: platform[1]?.x/level.map.tileWidth || platform[0].x/level.map.tileWidth,
            endY: platform[1]?.y/level.map.tileHeight || platform[0].y/level.map.tileHeight,
            name: platform[0].name,
            speed: platDur || 1,
            depth,
            ptype: platform[0].type || "default"
        }
        yield new Platform(platConfig)

    }
}
