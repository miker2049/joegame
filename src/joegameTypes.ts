
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

export type GameObjectInWorld = Phaser.GameObjects.GameObject & {
    //we can use this to see what things we add to generic gameobject in our game
    x: number
    y: number
    body: Phaser.Physics.Arcade.Body
    scale: number
}
