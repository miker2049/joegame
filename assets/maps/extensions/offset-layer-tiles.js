
function offsetTiles(layer, offset) {
    const edit = layer.edit()
    for (let y = 0; y < layer.height; y++) {
        for (let x = 0; x < layer.width; x++) {
            let tile = layer.tileAt(x, y)
            if (tile) {
                edit.setTile(x, y, tile.tileset.tile(tile.id+offset))
            }
        }
    }
    edit.apply()
}
