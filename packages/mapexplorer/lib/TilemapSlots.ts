import { Viewport } from "pixi-viewport";
import { Container, ContainerChild, Graphics } from "pixi.js";
import { DefaultParameters, SetCurrentMapFunction } from "./types";
import { config } from "./config";

type TilemapSlotsParameters = {
    x: number;
    y: number;
    parent: Container<ContainerChild>;
} & DefaultParameters;

export class TilemapSlots {
    x: number;
    y: number;
    parent: Container<ContainerChild>;
    viewport: Viewport;
    amount = 8;
    size = 32;
    setCurrentMap: SetCurrentMapFunction;
    constructor({
        parent,
        viewport,
        x,
        y,
        setCurrentMap,
    }: TilemapSlotsParameters) {
        this.x = x;
        this.y = y;
        this.parent = parent;
        this.viewport = viewport;
        this.setCurrentMap = setCurrentMap;

        for (let y = 0; y < this.amount; y++)
            for (let x = 0; x < this.amount; x++)
                this.drawRect(
                    (this.x + x) * this.size,
                    (this.y + y) * this.size,
                );
    }

    drawRect(x: number, y: number) {
        const g = new Graphics({});
        g.interactive = true;

        const draw = () => {
            g.clear();
            g.rect(x, y, this.size, this.size);
            g.fill({
                color: 0x110000,
                alpha: 0.0,
            });
        };
        if (config.drawTilemapGrid) {
            g.on("mouseover", () => {
                g.clear();
                draw();
                g.stroke(0xffffff);
                g.zIndex = 100;
            });
            g.on("mouseout", () => {
                g.clear();
                draw();
                g.stroke(0x00ff00);
                g.zIndex = 0;
            });
        }
        g.on("click", (ev) => {
            if (ev.button === 1) return;
            this.setQString(
                this.viewport.corner.x,
                this.viewport.corner.y,
                this.viewport.scale.x,
            );
            this.setCurrentMap(this.x / 256, this.y / 256, x / 32, y / 32).then(
                (tm) => {
                    // console.log("in slots: ", tm);
                    // tm.scale = 1 / 64;
                    // tm.x = x;
                    // tm.y = y;
                    // this.parent.addChild(tm);
                },
            );
            // window.location.href = `/maptest?x=${this.x / 256}&y=${this.y / 256}&file=${x / 32}&rank=${y / 32}`;
        });
        g.eventMode = "dynamic";
        draw();
        g.stroke(0x00ff00);
        this.parent.addChild(g);
    }

    setQString(x: number, y: number, z: number) {
        // Get current URL parts
        const path = window.location.pathname;
        const params = new URLSearchParams(window.location.search);
        const hash = window.location.hash;

        // Update query string values
        params.set("x", x.toString());
        params.set("y", y.toString());
        params.set("z", z.toString());

        // Update URL
        window.history.replaceState(
            {},
            "",
            `${path}?${params.toString()}${hash}`,
        );
    }
}
