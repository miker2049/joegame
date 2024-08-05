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
        this.tls = Array(8)
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
            worldWidth: 256 ** 2,
            worldHeight: 256 ** 2,

            events: this.app.renderer.events, // the interaction module is important for wheel to work properly when renderer.view is placed or scaled
        });

        this.tls.forEach((tl) => viewport.addChild(tl));
        // add the viewport to the stage
        this.app.stage.addChild(viewport);

        // activate plugins
        viewport
            .drag()
            .pinch()
            .wheel({ percent: 1 / 2 ** 8, lineHeight: 1, smooth: 23 })
            .decelerate();
        viewport.setZoom(2 / 256);
        viewport.on("moved", ({ viewport }) => {
            const { x, y } = viewport.corner;
            this.tls.forEach((tl) => tl.update(x, y, viewport.scale.x));
        });
        this.tls.forEach((tl) =>
            tl.update(viewport.corner.x, viewport.corner.y, viewport.scale.x),
        );
    }
}
