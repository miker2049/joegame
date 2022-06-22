"use strict";
exports.__esModule = true;
exports.parsewikidata = exports.createTmpData = void 0;
var wikientries;
(function (wikientries) {
    wikientries[wikientries["character"] = 0] = "character";
    wikientries[wikientries["spritesheet"] = 1] = "spritesheet";
    wikientries[wikientries["image"] = 2] = "image";
    wikientries[wikientries["platform"] = 3] = "platform";
    wikientries[wikientries["mapobject"] = 4] = "mapobject";
    wikientries[wikientries["convoManifest"] = 5] = "convoManifest";
    wikientries[wikientries["animatedTile"] = 6] = "animatedTile";
})(wikientries || (wikientries = {}));
var createTmpData = function () {
    return {
        spritesheet: new Map(),
        character: new Map(),
        image: new Map(),
        platform: new Map(),
        sound: new Map(),
        mapobject: new Map(),
        animatedTiles: new Map(),
        convoManifest: ''
    };
};
exports.createTmpData = createTmpData;
function parsewikidata(rawwikidata) {
    var tmpdata = (0, exports.createTmpData)();
    rawwikidata.forEach(function (page) {
        page.forEach(function (item) {
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
                    case wikientries[wikientries.animatedTile]:
                        tmpdata.mapobject.set(item.name, item);
                        break;
                }
            }
        });
    });
    return tmpdata;
}
exports.parsewikidata = parsewikidata;
