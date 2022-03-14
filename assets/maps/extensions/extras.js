
function alignObjectTool() {


    let action = tiled.registerAction("align-object",()=>{
        let tiled
        if (tiled.activeAsset.selectedObjects.length>0) {
            tiled.activeAsset.selectedObjects.forEach((object)=>{

                object.x = closestMultiple(object.x, tiled.activeAsset.tileWidth)
                object.y = closestMultiple(object.y, tiled.activeAsset.tileHeight)
                object.width = closestMultiple(object.width, tiled.activeAsset.tileWidth)
                object.height = closestMultiple(object.height, tiled.activeAsset.tileHeight)
            })
        } else {
            tiled.log('no objects selectedddd!!')
        }
    })

    action.shortcut = 'Ctrl-K'
    action.text= 'Align object to tilemap'

    tiled.extendMenu("Edit", [

        { action: "align-object", before: "SelectAll"  },
        { separator: true  }
    ]);


}


function getRegionHexTool() {
    /**
     * The qRect thing tile uses gives a simple object with
     * x,y,width,height.  We need to take that and then give back
     * some kind of list of coordinates.
     * */
    function getTilesFromRect(rect) {
        let out = []
        for (var y = 0; y < rect.height; ++y) {
            for (var x = 0; x < rect.width; ++x) {
                out.push([rect.x + x, rect.y + y])
            }
        }
        return out
    }
    /**
     * Takes a list of coords and a map width and height and returns a binary
     * string where the inputed coords are 1 and everything else is 0.*/
    function getBitMask(coords, width, height) {
        let out = Array(width * height).fill(0)
        coords.forEach(coord => {
            const ind = (coord[1] * width) + coord[0]
            out[ind] = 1
        })
        return out.join('')
    }

    let action = tiled.registerAction("set-region-hex-prop", () => {

        if (!tiled.activeAsset.isTileMap) {
            tiled.log("Not in a tilemap")
            return
        }
        const aa = tiled.activeAsset

        const rects = tiled.activeAsset.selectedArea.get().rects

        if (rects.length < 1){
            tiled.log("Not region selected.")
            return
        }

        let coords = []
        rects.forEach(item => {
            coords = coords.concat(getTilesFromRect(item))
        });
        const mask= getBitMask(coords, aa.tileWidth,aa.tileHeight)
        let hex = parseInt(mask,2).toString(16)

        const paddingn = ((aa.tileWidth/4)*aa.tileHeight)-hex.length
        tiled.log(paddingn)
        const padding = Array(paddingn).fill(0)
        hex = padding.join('') + hex
        const prop = tiled.prompt("Select a property")
        tiled.log(hex)
        aa.setProperty(prop,hex)
    })


    // action.shortcut = 'Ctrl-K'
    action.text= 'Set Property with Hex of selected region'

    tiled.extendMenu("Edit", [

        { action: "set-region-hex-prop", before: "align-object"  },
        { separator: true  }
    ]);

}

alignObjectTool()
getRegionHexTool()
