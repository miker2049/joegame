const Tiled = require('@mapeditor/tiled-api')





let action = tiled.registerAction("create-desert-walls",()=>{
    let tiled
    if (tiled.activeAsset.selectedObjects.length>0) {
        tiled.activeAsset.selectedObjects.forEach((object)=>{

            object.x = closestMultiple(object.x, tiled.activeAsset.tileWidth)
            object.y = closestMultiple(object.y, tiled.activeAsset.tileHeight)
            object.width = closestMultiple(object.width, tiled.activeAsset.tileWidth)
            object.height = closestMultiple(object.height, tiled.activeAsset.tileHeight)
        })
    } else {
        tiled.log('no objects selected!!')
    }
})

action.shortcut = 'Ctrl-K'
action.text= 'Align object to tilemap'

tiled.extendMenu("Edit", [
    { action: "aligntile", before: "SelectAll"  },
    { separator: true  }
]);
