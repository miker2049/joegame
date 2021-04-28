var wikientries;
(function (wikientries) {
    wikientries[wikientries["character"] = 0] = "character";
    wikientries[wikientries["spritesheet"] = 1] = "spritesheet";
    wikientries[wikientries["image"] = 2] = "image";
    wikientries[wikientries["platform"] = 3] = "platform";
    wikientries[wikientries["mapobject"] = 4] = "mapobject";
    wikientries[wikientries["convoManifest"] = 5] = "convoManifest";
})(wikientries || (wikientries = {}));
export const createTmpData = () => {
    return {
        spritesheet: new Map(),
        character: new Map(),
        image: new Map(),
        platform: new Map(),
        mapobject: new Map(),
        convoManifest: ''
    };
};
export function parsewikidata(rawwikidata) {
    let tmpdata = createTmpData();
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
    return tmpdata;
}
//# sourceMappingURL=parseWikiData.js.map