function getKeyName(path) {
    return path.match(/[\w-]+\./)[0].replace(/\./, "");
}
function getSceneKeyName(path) {
    return getKeyName(path) + "_scene";
}
function getDialogueKeyName(path) {
    return getKeyName(path) + "_dialogue";
}
function getMapKeyName(path) {
    return getKeyName(path) + "_map";
}
function getMapKeyNameRaw(path) {
    return getMapKeyName(path) + "_raw";
}
export { getDialogueKeyName, getMapKeyName, getMapKeyNameRaw, getKeyName, getSceneKeyName };
//# sourceMappingURL=getKeyNames.js.map