
function getKeyName(path:string){
    return path.match(/[\w-]+\./)![0].replace(/\./,"")
}

function getSceneKeyName(path:string){
    return  getKeyName(path) + "_scene"
}

function getDialogueKeyName(path:string){
    return  getKeyName(path) + "_dialogue"
}

function getMapKeyName(path:string){
    return  getKeyName(path) + "_map"
}

function getMapKeyNameRaw(path:string){
    return getMapKeyName(path) + "_raw"
}

export { getDialogueKeyName, getMapKeyName,getMapKeyNameRaw, getKeyName, getSceneKeyName }
