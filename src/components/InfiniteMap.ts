import 'phaser'
import { Axis, Dir } from '../joegameTypes'
import { getMapKeyNameRaw } from '../utils/getKeyNames'
import createTilemap from'../factories/createTilemap'
import TiledRawJson from'../../typings/TiledRawJson'
import { IMap } from '../ILevel'
import getTileFromPoint from '../utils/getTileFromPoint'



interface IInfiniteMapParams {
    scene: Phaser.Scene
    tilemap: Phaser.Tilemaps.Tilemap
    mapObjects: Phaser.GameObjects.Image[]
}

interface IInfiniteMap {
    shiftNorth(): void
    shiftSouth(): void
    shiftEast(): void
    shiftWest(): void
}

interface IMapObjectWatcher {
    watchNorth: number
    watchSouth: number
    watchEast: number
    watchWest: number
    obj: Phaser.GameObjects.Image
    shift(dir: Dir): void
    check(x: number, y: number): void
}

export class MapObjectWatcher implements IMapObjectWatcher {
    obj: Phaser.GameObjects.Image
    tileSize: number
    mapTWidth: number
    mapTHeight: number
    limit: number

    constructor(obj: Phaser.GameObjects.Image, limit: number, tileSize: number, mapTWidth: number, mapTHeight: number){
        this.obj = obj
        this.tileSize = tileSize
        this.mapTWidth = mapTWidth
        this.mapTHeight = mapTHeight
        //in tile units
        this.limit = limit

        //these are in tile units
        // this.watchNorth = Math.round((this.obj.y - (this.mapTHeight*this.tileSize))/this.tileSize)+limit
        // this.watchSouth = Math.round(((this.obj.y+this.obj.height) + (this.mapTHeight*this.tileSize))/this.tileSize)-limit
        // this.watchEast = Math.round((this.obj.x + (this.mapTWidth*this.tileSize))/this.tileSize)-limit
        // this.watchWest = Math.round(((this.obj.x+this.obj.width) - (this.mapTWidth*this.tileSize))/this.tileSize)+limit

    }
    shift(dir: Dir): void {
        switch(dir){
            case Dir.north: {
                this.obj.y -= this.mapTHeight * this.tileSize
                // this.watchNorth = (this.obj.y + this.limit)/this.tileSize
                break
            }
            case Dir.south: {
                this.obj.y += this.mapTHeight * this.tileSize
                // this.watchNorth = (this.obj.y + this.limit)/this.tileSize
                break
            }
            case Dir.east: {
                this.obj.x += this.mapTWidth * this.tileSize
                // this.watchNorth = (this.obj.y + this.limit)/this.tileSize
                break
            }
            case Dir.west: {
                this.obj.x -= this.mapTWidth * this.tileSize
                // this.watchNorth = (this.obj.y + this.limit)/this.tileSize
                break
            }
        }
    }

    get watchNorth(): number {
        return Math.round((this.obj.y - (this.mapTHeight*this.tileSize))/this.tileSize)+this.limit
    }
    get watchSouth(): number {
        return Math.round(((this.obj.y+this.obj.height) + (this.mapTHeight*this.tileSize))/this.tileSize)-this.limit
    }
    get watchEast(): number {
        return Math.round((this.obj.x + (this.mapTWidth*this.tileSize))/this.tileSize)-this.limit
    }
    get watchWest(): number {
        return Math.round(((this.obj.x+this.obj.width) - (this.mapTWidth*this.tileSize))/this.tileSize)+this.limit
    }

    check(x: number, y: number): void {
        console.log('y',y,'this.watchNorth',this.watchNorth)
        if(x<=this.watchWest){
            this.shift(Dir.west)
        } else if (x>=this.watchEast){
            this.shift(Dir.east)
        }else if (y<=this.watchNorth){
            this.shift(Dir.north)
        }else if (y>=this.watchSouth){
            this.shift(Dir.south)
        }
    }
}

export class InfiniteMap implements IMap, IInfiniteMap {
    tilemap: Phaser.Tilemaps.Tilemap
    cameraCenter: Phaser.Types.Math.Vector2Like
    tileOffsetX: number
    tileOffsetY: number
    tileSize: number
    mapTileWidth: number
    mapTileHeight: number
    objWatchers: IMapObjectWatcher[]

    constructor(params: IInfiniteMapParams){
        this.tilemap = params.tilemap
        const camera = params.scene.cameras.main
        this.tileOffsetX = 0
        this.tileOffsetY = 0

        // let raw=this.params.scene.cache.json.get(getMapKeyNameRaw(this.params.tilemapPath)) as TiledRawJson
        this.mapTileWidth = this.tilemap.width
        this.mapTileHeight = this.tilemap.height
        this.tileSize = this.tilemap.tileWidth


        this.objWatchers = params.mapObjects.map((mo)=>new MapObjectWatcher(mo, 4, this.tileSize, this.tilemap.width, this.tilemap.height))
        this.cameraCenter = getTileFromPoint(camera.midPoint,this.tilemap.tileWidth)

        camera.on('followupdate',(cam,gob)=>{
            const newmid =  getTileFromPoint(camera.midPoint,this.tilemap.tileWidth)
            if (typeof this.cameraCenter.x === 'number' &&
                typeof this.cameraCenter.y === 'number' &&
                typeof newmid.x === 'number' &&
                typeof newmid.y === 'number' ) {
                if ( newmid.y < this.cameraCenter.y ){
                    this.cameraCenter.y = newmid.y
                    this.shiftNorth()
                    this.objWatchers.forEach((w)=>w.check(newmid.x, newmid.y))
                } else if ( newmid.y > this.cameraCenter.y){
                    this.cameraCenter.y = newmid.y
                    this.shiftSouth()
                    this.objWatchers.forEach((w)=>w.check(newmid.x, newmid.y))
                } else if ( newmid.x > this.cameraCenter.x ){
                    this.cameraCenter.x = newmid.y
                    this.shiftEast()
                    this.objWatchers.forEach((w)=>w.check(newmid.x, newmid.y))
                } else if ( newmid.x < this.cameraCenter.x ){
                    this.cameraCenter.x = newmid.x
                    this.shiftWest()
                    this.objWatchers.forEach((w)=>w.check(newmid.x, newmid.y))
                }
            }
        })
    }

    get tileWidth(): number {
        return this.tilemap.tileWidth
    }

    get tileHeight(): number {
        return this.tilemap.tileHeight
    }

    get height(): number {
        return this.tilemap.height
    }

    get width(): number {
        return this.tilemap.width
    }

    get tilesets(): Phaser.Tilemaps.Tileset[] {
        return this.tilemap.tilesets
    }

    getObjectLayer(layer: string): Phaser.Tilemaps.ObjectLayer {
        return this.tilemap.getObjectLayer(layer)
    }

    getLayer(layer: string): Phaser.Tilemaps.LayerData {
        return this.tilemap.getLayer(layer)
    }

    getTileAt(x: number, y: number, nonNull: boolean, layer: string): {index: number, properties?: {collides?: boolean}} {
        return this.tilemap.getTileAt(x,y,nonNull,layer)
    }

    shiftNorth(): void {
        this.tilemap.layers.forEach((l)=>{
            console.log(this.mapTileHeight-1)
            l.tilemapLayer.copy(0,this.mapTileHeight-1,this.mapTileWidth,1,0,0)
        })
    }
    shiftSouth(): void {}
    shiftEast(): void {}
    shiftWest(): void {}

}
