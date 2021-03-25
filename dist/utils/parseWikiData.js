var wikientries;
(function (wikientries) {
    wikientries[wikientries["character"] = 0] = "character";
    wikientries[wikientries["spritesheet"] = 1] = "spritesheet";
    wikientries[wikientries["image"] = 2] = "image";
    wikientries[wikientries["platform"] = 3] = "platform";
    wikientries[wikientries["mapobject"] = 4] = "mapobject";
})(wikientries || (wikientries = {}));
export function parsewikidata(rawwikidata) {
    let tmpdata = {
        spritesheet: new Map(),
        character: new Map(),
        image: new Map(),
        platform: new Map(),
        mapobject: new Map()
    };
    rawwikidata.forEach((page) => {
        page.forEach((item) => {
            if (item.type) {
                switch (item.type) {
                    case wikientries[wikientries.character]:
                        tmpdata.character.set(item.name, item);
                        break;
                    case wikientries[wikientries.spritesheet]:
                        tmpdata.spritesheet.set(item.key, item);
                        break;
                    case wikientries[wikientries.image]:
                        tmpdata.image.set(item.key || item.name, item);
                        break;
                    case wikientries[wikientries.platform]:
                        tmpdata.platform.set(item.name, item);
                        break;
                    case wikientries[wikientries.mapobject]:
                        tmpdata.mapobject.set(item.name, item);
                        break;
                }
            }
        });
    });
    // console.log(JSON.stringify(tmpdata, function(key, value) {
    //     if (value instanceof Map) {
    //         return {
    //             dataType: 'Map',
    //             value: Array.from(value.entries()), // or with spread: value: [...value]
    //         };
    //     } else {
    //         return value;
    //     }
    // }))
    return tmpdata;
}
//# sourceMappingURL=parseWikiData.js.map