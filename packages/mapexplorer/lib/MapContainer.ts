import { Container, Application } from "pixi.js";
import { TileLayer } from "./TileLayer";
import { Viewport } from "pixi-viewport";
import { TileCache } from "./utils";

export class MapContainer extends Container {
    private tls: TileLayer[];
    private cache: TileCache;

    constructor(private app: Application) {
        super();
        this.cache = new TileCache(10 ** 4);
        this.tls = Array(12)
            .fill(0)
            .map(
                (_, idx) =>
                    new TileLayer({
                        screenWidth: app.screen.width,
                        screenHeight: app.screen.height,
                        tileSize: 256,
                        zoomLevel: idx,
                        tcache: this.cache,
                    }),
            );
        this.initViewport();
    }
    private initViewport() {
        // create viewport
        const viewport = new Viewport({
            screenWidth: window.innerWidth,
            screenHeight: window.innerHeight,
            worldWidth: 2 ** 16,
            worldHeight: 2 ** 16,
            events: this.app.renderer.events, // the interaction module is important for wheel to work properly when renderer.view is placed or scaled
        });

        this.tls.forEach((tl) => viewport.addChild(tl));
        // add the viewport to the stage
        this.app.stage.addChild(viewport);
        const modeline: HTMLInputElement | null =
            document.querySelector("#modeline-input");
        if (!modeline) console.error("no modeline div");

        // activate plugins
        viewport
            .clampZoom({ maxWidth: 2 ** 16, maxHeight: 2 ** 16 })
            .clamp({
                options: {
                    left: true,
                    right: false,
                    top: true,
                    bottom: false,
                },
                underflow: "none",
                direction: "all",
            })
            .drag()
            .pinch()
            .wheel({ percent: 1 / 2 ** 8, lineHeight: 1, smooth: 23 })
            .decelerate();
        viewport.setZoom(1 / 32);
        viewport.on("moved", ({ viewport }) => {
            const { x, y } = viewport.corner;
            this.tls.forEach((tl) => tl.update(x, y, viewport.scale.x));
            if (modeline)
                modeline.value = `${Math.floor(x)} , ${Math.floor(y)}, ${viewport.scale.x}`;
        });
        this.tls.forEach((tl) =>
            tl.update(viewport.corner.x, viewport.corner.y, viewport.scale.x),
        );
    }
}
