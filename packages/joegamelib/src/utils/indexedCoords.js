"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.coordsToIndex = exports.indexToCoords = void 0;
function indexToCoords(n, w) {
    return [
        n % w,
        Math.floor(n / w)
    ];
}
exports.indexToCoords = indexToCoords;
function coordsToIndex(x, y, width) {
    return (y * width) + x;
}
exports.coordsToIndex = coordsToIndex;
//# sourceMappingURL=indexedCoords.js.map