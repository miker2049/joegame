interface Tilestamp {
    tiles: Array<number | null>
    width: number
}

interface NineSliceCornerConfig {
    topLeft: Tilestamp
    topRight: Tilestamp
    bottomLeft: Tilestamp
    bottomRight: Tilestamp
}

interface NineSliceStuffingConfig {
    left: Tilestamp
    right: Tilestamp
    bottom: Tilestamp
    top: Tilestamp
}

interface NineSliceConfig {
    corners: NineSliceCornerConfig
    stuffing: NineSliceStuffingConfig
    center: Tilestamp | Tilestamp[]

}

/*
 * in2 is overlayed onto in1, with offset. They are both copied to
 * a fresh array.
 */
function addStampToStamp(in1: Tilestamp, in2: Tilestamp,
    xOff: number, yOff: number): Tilestamp {
    let width = 8
    let out: Tilestamp = {
        tiles: Array(width*width),
        width: width
    }
    return out
}
