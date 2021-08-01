
export enum Dir {
    north = "north",
    south = "south",
    east = "east",
    west = "west"
}


export const VelocityMap = {
    north: [0, -1],
    south: [0, 1],
    east: [1, 0],
    west: [-1, 0]
}


export enum Axis {
    xaxis,
    yaxis
}

export type InterestSet = { x: number, y: number }[]


export interface CharMoveAnims {
    north: string
    south: string
    east: string
    west: string
}

export type AssuredVec2 = { x: number, y: number }
