
const chromaticScale = [
    1,
    1.059,
    1.122,
    1.189,
    1.260,
    1.334,
    1.414,
    1.498,
    1.587,
    1.682,
    1.782,
    1.888
]

const majorScale = [
    chromaticScale[0],
    chromaticScale[2],
    chromaticScale[4],
    chromaticScale[5],
    chromaticScale[7],
    chromaticScale[9],
    chromaticScale[11],
]

const pentaMajorScale = [
    chromaticScale[0],
    chromaticScale[2],
    chromaticScale[5],
    chromaticScale[7],
    chromaticScale[9],
]

const harmonicMinorScale = [
    chromaticScale[0],
    chromaticScale[2],
    chromaticScale[3],
    chromaticScale[5],
    chromaticScale[7],
    chromaticScale[8],
    chromaticScale[11],
]

const itemScale = [
    chromaticScale[0],
    chromaticScale[2],
    chromaticScale[5],
    chromaticScale[7],
    chromaticScale[8],
]

export default {
    chromaticScale,
    itemScale,
    majorScale,
    harmonicMinorScale,
    pentaMajorScale
}
