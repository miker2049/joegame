const decom = require("./parse-tiled-zlib")
// Bits on the far end of the 32-bit global tile ID are used for tile flags
const FLIPPED_HORIZONTALLY_FLAG = 0x80000000;
const FLIPPED_VERTICALLY_FLAG   = 0x40000000;
const FLIPPED_DIAGONALLY_FLAG   = 0x20000000;
const FLIPPED_FLAGS = FLIPPED_DIAGONALLY_FLAG | FLIPPED_VERTICALLY_FLAG | FLIPPED_HORIZONTALLY_FLAG

function clearFlipping(n){
    return n & ~FLIPPED_FLAGS
}

function getFlips(n){
    return {
        flippedHorizontally: (n & FLIPPED_HORIZONTALLY_FLAG) != 0,
        flippedVertically: (n & FLIPPED_VERTICALLY_FLAG) != 0,
        flipDiagonally: (n & FLIPPED_DIAGONALLY_FLAG) != 0
    }
}
