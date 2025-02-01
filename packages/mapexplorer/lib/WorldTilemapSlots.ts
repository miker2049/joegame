import { Viewport } from "pixi-viewport";
import { Container, ContainerChild, Graphics } from "pixi.js";
import { DefaultParameters, SetCurrentMapFunction } from "./types";
import { config } from "./config";

type WorldTilemapSlotsParameters = {
    x: number;
    y: number;
    parent: Container<ContainerChild>;
} & DefaultParameters;

export class WorldTilemapSlots {
    x: number;
    y: number;
    parent: Container<ContainerChild>;
    viewport: Viewport;
    amount = 8;
    size = 32;
    setCurrentMap: SetCurrentMapFunction;
    currActive:
        | { x: number; y: number; file: number; rank: number }
        | undefined;
    constructor({
        parent,
        viewport,
        x,
        y,
        setCurrentMap,
    }: WorldTilemapSlotsParameters) {
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
                alpha: 0.4,
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
            if (this.currActive?.file === x && this.currActive?.rank === y)
                return;
            this.setQString(
                this.viewport.corner.x,
                this.viewport.corner.y,
                this.viewport.scale.x,
            );
            this.setCurrentMap(this.x / 256, this.y / 256, x / 32, y / 32).then(
                (tm) => {
                    if (tm) {
                        this.currActive = {
                            x: this.x,
                            y: this.y,
                            file: x,
                            rank: y,
                        };

                        tm.on("destroyed", () => (this.currActive = undefined));
                    }
                },
            );
        });
        g.eventMode = "dynamic";
        draw();
        g.stroke({ color: 0x00ff00, alpha: 1.0 });
        this.parent.addChild(g);
    }

    setQString(x: number, y: number, z: number) {
        // Get current URL parts
        const path = window.location.pathname;
        const params = new URLSearchParams(window.location.search);
        const hash = window.location.hash;

        // Update query string values
        params.set("x", Math.floor(x).toString());
        params.set("y", Math.floor(y).toString());
        params.set("z", Math.floor(z).toString());

        // Update URL
        window.history.replaceState(
            {},
            "",
            `${path}?${params.toString()}${hash}`,
        );
    }

    update(x: number, y: number, z: number) {
        // console.log(this.viewport.getVisibleBounds());
    }
}
