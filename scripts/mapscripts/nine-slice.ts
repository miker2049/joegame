/**
 * # nine-slice functions
 *
 * ## defining a nine slice
 * This is done through a tilemap, but in order to show which parts of the
 * tilemap are which slice, you define each slice with a number whose bits
 * are set in order from the top left to the bottom right, reading it like a book
 *
 * ```
 * 1000
 * 1000
 * 0000
 * 0000
 * ```
 * Would be a possible mask for a topLeft
 * - 1. topLeft
 * - 2. top
 * - 3. topRight
 * - 4. left
 * - 5. center
 * - 6. right
 * - 7. bottomLeft
 * - 8. bottom
 * - 9. bottomRight
 *
 * These are defined at a map level as a hex string.  This way we can account for any
 * amount of combinations.
 * You can easily find new combos by, e.g.
 * ```js
 * parseInt('0000 0000 0001 0001'.replaceAll(' ',''), 2).toString(16)
 * ```
 * But this does not take care of padding... Which *is* important here,
 * as we need it to deterministicly get our region.
 *
 * The number of digits in our hex regions should equal (tileWidth/4) * tileHeight
 */

import { unflat } from "./mapscript-utils"

type StampTile = number | null

type Tilestamp = StampTile[][]

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

/**
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


function nineSliceGenerate(l: number, w: number, sliceConfig: NineSliceConfig): Tilestamp {
    let out = unflat(Array(l*w).fill(0),w)


    return out
}
