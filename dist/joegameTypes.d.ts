export declare enum Dir {
    north = "north",
    south = "south",
    east = "east",
    west = "west"
}
export declare const VelocityMap: {
    north: number[];
    south: number[];
    east: number[];
    west: number[];
};
export declare enum Axis {
    xaxis = 0,
    yaxis = 1
}
export declare type InterestSet = {
    x: number;
    y: number;
}[];
export interface CharMoveAnims {
    north: string;
    south: string;
    east: string;
    west: string;
}
