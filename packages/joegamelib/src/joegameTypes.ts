import Phaser from 'phaser'
export enum Dir {
  north = 'north',
  south = 'south',
  east = 'east',
  west = 'west'
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

export type InterestSet = AssuredVec2[]

export interface CharMoveAnims {
  north: number[]
  south: number[]
  east: number[]
  west: number[]
}

export type AssuredVec2 = { x: number; y: number }

// Phaser.Ty

export type GameObjectInWorld =
  Phaser.Types.Physics.Arcade.SpriteWithDynamicBody
type t = Phaser.Types.Physics.Arcade.SpriteWithDynamicBody
