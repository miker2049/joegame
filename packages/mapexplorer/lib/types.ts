import { Application, Container, EventEmitter } from "pixi.js";
import { TileCache } from "./utils";
import { Viewport } from "pixi-viewport";

export type TwoNums = [number, number];
export type Pnt = TwoNums;
export type MapAddress = [number, number, number, number];
export type SetCurrentMapFunction = (
    x: number,
    y: number,
    file: number,
    rank: number,
) => Promise<Container | undefined>;

export type DefaultParameters = {
    tileSize: number;
    tcache: TileCache;
    app: Application;
    viewport: Viewport;
    setCurrentMap: SetCurrentMapFunction;
    zoomLevel: number;
    events: EventEmitter;
};

export type BaseLayer = {
    update: (x: number, y: number, z: number) => void;
} & Container;
