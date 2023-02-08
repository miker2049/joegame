type PropertyType =
  | 'string'
  | 'int'
  | 'float'
  | 'bool'
  | 'color'
  | 'file'
  | 'object'
  | 'class'
interface Export {
  format: string
  target: string
}

interface Editorsettings {
  export: Export
}

interface TiledJsonProperty {
  name: string
  type: PropertyType
  value: any
}

export interface TiledJsonObject {
  charGroup: string
  height: number
  id: number
  name: string
  properties: TiledJsonProperty[]
  rotation: number
  type: string
  visible: boolean
  width: number
  x: number
  y: number
  gid?: number
  point?: boolean
}

export interface IObjectLayer extends BaseLayer {
  objects: TiledJsonObject[]
  type: 'objectgroup'
}

export interface ITileLayer extends BaseLayer {
  height: number
  width: number
  data: number[]
  type: 'tilelayer'
}

export interface BaseLayer {
  id: number
  name: string
  opacity: number
  properties: TiledJsonProperty[]
  type: string
  visible: boolean
  x: number
  y: number
  draworder: string
}

type ILayer = IObjectLayer | ITileLayer

interface TileAnimation {
  duration: number
  tileid: number
}

interface Tile {
  id: number
  properties: TiledJsonProperty[]
  image: string
  imageheight: number
  imagewidth: number
  animation?: TileAnimation[]
  objectgroup?: TileObjectGroup
}

interface TileObjectGroup {
  id: number
  objects: {
    height: number
    width: number
    x: number
    y: number
  }[]
}
interface Tileset {
  columns: number
  firstgid: number
  image: string
  imageheight: number
  imagewidth: number
  margin: number
  name: string
  spacing: number
  tilecount: number
  tileheight: number
  tiles: Tile[]
  tilewidth: number
  source?: string
}

export default interface TiledRawJSON {
  compressionlevel: number
  editorsettings: Editorsettings
  height: number
  infinite: boolean
  layers: ILayer[]
  nextlayerid: number
  nextobjectid: number
  orientation: string
  properties: TiledJsonProperty[]
  renderorder: string
  tiledversion: string
  tileheight: number
  tilesets: Tileset[]
  tilewidth: number
  type: string
  version: number
  width: number
}
