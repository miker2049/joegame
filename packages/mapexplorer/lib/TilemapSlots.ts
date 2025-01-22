import { Container, ContainerChild, Graphics } from "pixi.js";

export class TilemapSlots {
    x: number;
    y: number;
    g: Graphics;
    amount = 8;
    constructor(
        x: number,
        y: number,
        private size: number,
        private parent: Container<ContainerChild>,
    ) {
        this.x = x;
        this.y = y;

        for (let y = 0; y < this.amount; y++)
            for (let x = 0; x < this.amount; x++)
                this.drawRect((this.x + x) * size, (this.y + y) * this.size);
        console.log("SLOTS", this.x, this.y);
    }

    drawRect(x: number, y: number) {
        const g = new Graphics({});
        g.interactive = true;

        const draw = () => {
            g.clear();
            g.rect(x, y, this.size, this.size);
            g.fill({
                color: 0x110000,
                alpha: 0.3,
            });
        };
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
        g.eventMode = "dynamic";
        draw();
        g.stroke(0x00ff00);
        this.parent.addChild(g);
    }
}
