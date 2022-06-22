export default interface TiledWorldJson {
    maps: { filename: string, height: number, width: number, x: number, y: number }[]
    onlyShowAdjacentMaps: boolean
    type: string
}
