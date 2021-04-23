import Platform from '../components/Platform';
export default function* (level, layer) {
    var _a, _b;
    if (!level.map.getObjectLayer(layer))
        return;
    let platformSets = {};
    for (let obj_ of level.map.getObjectLayer(layer).objects) {
        if (platformSets[obj_.name]) {
            platformSets[obj_.name].push(obj_);
        }
        else {
            platformSets[obj_.name] = [obj_];
        }
    }
    // now we iterate through our new object and create the npcs
    for (let plat in platformSets) {
        const platform = platformSets[plat];
        let platDur = 1000;
        if (platform[0].properties) {
            platform[0].properties.forEach((prop) => {
                if (prop.name === 'speed') {
                    platDur = prop.value;
                }
            });
        }
        const platConfig = {
            level: level,
            x: platform[0].x / level.map.tileWidth,
            y: platform[0].y / level.map.tileHeight,
            width: platform[0].width / level.map.tileWidth,
            height: platform[0].height / level.map.tileHeight,
            endX: ((_a = platform[1]) === null || _a === void 0 ? void 0 : _a.x) / level.map.tileWidth || platform[0].x / level.map.tileWidth,
            endY: ((_b = platform[1]) === null || _b === void 0 ? void 0 : _b.y) / level.map.tileHeight || platform[0].y / level.map.tileHeight,
            name: platform[0].name,
            speed: platDur || 1,
            ptype: platform[0].type || "default"
        };
        yield new Platform(platConfig);
    }
}
//# sourceMappingURL=createPlatformsFromLayer.js.map