import 'phaser';
import { Dir } from '../joegameTypes';
import getTileFromPoint from '../utils/getTileFromPoint';
export class MapObjectWatcher {
    constructor(obj, limit, tileSize, mapTWidth, mapTHeight) {
        this.obj = obj;
        this.tileSize = tileSize;
        this.mapTWidth = mapTWidth;
        this.mapTHeight = mapTHeight;
        //in tile units
        this.limit = limit;
        //these are in tile units
        // this.watchNorth = Math.round((this.obj.y - (this.mapTHeight*this.tileSize))/this.tileSize)+limit
        // this.watchSouth = Math.round(((this.obj.y+this.obj.height) + (this.mapTHeight*this.tileSize))/this.tileSize)-limit
        // this.watchEast = Math.round((this.obj.x + (this.mapTWidth*this.tileSize))/this.tileSize)-limit
        // this.watchWest = Math.round(((this.obj.x+this.obj.width) - (this.mapTWidth*this.tileSize))/this.tileSize)+limit
    }
    shift(dir) {
        switch (dir) {
            case Dir.north: {
                this.obj.y -= this.mapTHeight * this.tileSize;
                // this.watchNorth = (this.obj.y + this.limit)/this.tileSize
                break;
            }
            case Dir.south: {
                this.obj.y += this.mapTHeight * this.tileSize;
                // this.watchNorth = (this.obj.y + this.limit)/this.tileSize
                break;
            }
            case Dir.east: {
                this.obj.x += this.mapTWidth * this.tileSize;
                // this.watchNorth = (this.obj.y + this.limit)/this.tileSize
                break;
            }
            case Dir.west: {
                this.obj.x -= this.mapTWidth * this.tileSize;
                // this.watchNorth = (this.obj.y + this.limit)/this.tileSize
                break;
            }
        }
    }
    get watchNorth() {
        return Math.round((this.obj.y - (this.mapTHeight * this.tileSize)) / this.tileSize) + this.limit;
    }
    get watchSouth() {
        return Math.round(((this.obj.y + this.obj.height) + (this.mapTHeight * this.tileSize)) / this.tileSize) - this.limit;
    }
    get watchEast() {
        return Math.round((this.obj.x + (this.mapTWidth * this.tileSize)) / this.tileSize) - this.limit;
    }
    get watchWest() {
        return Math.round(((this.obj.x + this.obj.width) - (this.mapTWidth * this.tileSize)) / this.tileSize) + this.limit;
    }
    check(x, y) {
        console.log('y', y, 'this.watchNorth', this.watchNorth);
        if (x <= this.watchWest) {
            this.shift(Dir.west);
        }
        else if (x >= this.watchEast) {
            this.shift(Dir.east);
        }
        else if (y <= this.watchNorth) {
            this.shift(Dir.north);
        }
        else if (y >= this.watchSouth) {
            this.shift(Dir.south);
        }
    }
}
export class InfiniteMap {
    constructor(params) {
        this.tilemap = params.tilemap;
        const camera = params.scene.cameras.main;
        this.tileOffsetX = 0;
        this.tileOffsetY = 0;
        // let raw=this.params.scene.cache.json.get(getMapKeyNameRaw(this.params.tilemapPath)) as TiledRawJson
        this.mapTileWidth = this.tilemap.width;
        this.mapTileHeight = this.tilemap.height;
        this.tileSize = this.tilemap.tileWidth;
        this.objWatchers = params.mapObjects.map((mo) => new MapObjectWatcher(mo, 4, this.tileSize, this.tilemap.width, this.tilemap.height));
        this.cameraCenter = getTileFromPoint(camera.midPoint, this.tilemap.tileWidth);
        camera.on('followupdate', (cam, gob) => {
            const newmid = getTileFromPoint(camera.midPoint, this.tilemap.tileWidth);
            if (typeof this.cameraCenter.x === 'number' &&
                typeof this.cameraCenter.y === 'number' &&
                typeof newmid.x === 'number' &&
                typeof newmid.y === 'number') {
                if (newmid.y < this.cameraCenter.y) {
                    this.cameraCenter.y = newmid.y;
                    this.shiftNorth();
                    this.objWatchers.forEach((w) => w.check(newmid.x, newmid.y));
                }
                else if (newmid.y > this.cameraCenter.y) {
                    this.cameraCenter.y = newmid.y;
                    this.shiftSouth();
                    this.objWatchers.forEach((w) => w.check(newmid.x, newmid.y));
                }
                else if (newmid.x > this.cameraCenter.x) {
                    this.cameraCenter.x = newmid.y;
                    this.shiftEast();
                    this.objWatchers.forEach((w) => w.check(newmid.x, newmid.y));
                }
                else if (newmid.x < this.cameraCenter.x) {
                    this.cameraCenter.x = newmid.x;
                    this.shiftWest();
                    this.objWatchers.forEach((w) => w.check(newmid.x, newmid.y));
                }
            }
        });
    }
    get tileWidth() {
        return this.tilemap.tileWidth;
    }
    get tileHeight() {
        return this.tilemap.tileHeight;
    }
    get height() {
        return this.tilemap.height;
    }
    get width() {
        return this.tilemap.width;
    }
    get tilesets() {
        return this.tilemap.tilesets;
    }
    getObjectLayer(layer) {
        return this.tilemap.getObjectLayer(layer);
    }
    getLayer(layer) {
        return this.tilemap.getLayer(layer);
    }
    getTileAt(x, y, nonNull, layer) {
        return this.tilemap.getTileAt(x, y, nonNull, layer);
    }
    shiftNorth() {
        this.tilemap.layers.forEach((l) => {
            console.log(this.mapTileHeight - 1);
            l.tilemapLayer.copy(0, this.mapTileHeight - 1, this.mapTileWidth, 1, 0, 0);
        });
    }
    shiftSouth() { }
    shiftEast() { }
    shiftWest() { }
}
//# sourceMappingURL=InfiniteMap.js.map