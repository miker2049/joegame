import { Container, Application } from "pixi.js";
import { WorldTileLayer } from "./WorldTileLayer";
import { Viewport } from "pixi-viewport";
import { TileCache } from "./utils";
import { JTilemap } from "./JTilemap";

export class MapContainer extends Container {
    private tls: WorldTileLayer[];
    private cache: TileCache;

    private modeline: HTMLInputElement | null;
    viewport: Viewport;
    currentMap: Container | undefined;
    loadingMap = false;
    // private updateTimer: Return;

    constructor(private app: Application) {
        super();
        this.cache = new TileCache(10 ** 4);
        this.viewport = this.initViewport();
        this.tls = Array(9)
            .fill(0)
            .map(
                (_, idx) =>
                    new WorldTileLayer({
                        screenWidth: app.screen.width,
                        screenHeight: app.screen.height,
                        tileSize: 256,
                        zoomLevel: idx + 0,
                        tcache: this.cache,
                        app,
                        viewport: this.viewport,
                        setCurrentMap: this.setCurrentMap,
                    }),
            );
        this.tls.forEach((tl) => this.viewport.addChild(tl));
        // add the this.viewport to the stage
        this.app.stage.addChild(this.viewport);
        this.modeline = document.querySelector("#modeline-input");
        if (!this.modeline) console.error("no modeline div");
    }

    updateLayers(x: number, y: number, z: number) {
        this.tls.forEach((tl) => tl.update(x, y, z));
        if (this.modeline)
            this.modeline.value = `${Math.floor(x)} , ${Math.floor(y)}, ${z}`;
    }

    async setCurrentMap(x: number, y: number, file: number, rank: number) {
        if (this.loadingMap) return;
        try {
            this.loadingMap = true;
            if (this.currentMap) {
                if (this.currentMap.parent)
                    this.viewport.removeChild(this.currentMap);
                this.currentMap.destroy();
            }
            this.currentMap = await JTilemap.fetchMap(x, y, file, rank);
            this.currentMap.scale = 1 / 64;
            this.currentMap.x = x * 256 + file * 32;
            this.currentMap.y = y * 256 + rank * 32;
            this.viewport.addChild(this.currentMap);
            this.currentMap.on("removed", () => console.log("child removed!"));
            return this.currentMap;
        } catch (err) {
            console.log(err);
        } finally {
            this.loadingMap = false;
        }
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

        // activate plugins
        viewport
            .clampZoom({
                maxWidth: 2 ** 16,
                maxHeight: 2 ** 16,
                minWidth: 256 / 8 / 8,
                minHeight: 256 / 8 / 8,
            })
            .clamp({
                underflow: "none",
                direction: "all",
            })
            .drag({ mouseButtons: "middle" })
            .pinch()
            .wheel({ percent: 1 / 2 ** 16, lineHeight: 1, smooth: 23 })
            .decelerate();
        viewport.on("moved", ({ viewport }) => {
            const { x, y } = viewport.corner;
            this.updateLayers(x, y, viewport.scale.x);
        });

        return viewport;
    }

    static getParams() {
        const searchParams: URLSearchParams = new URLSearchParams(
            window.location.search,
        );
        const paramX = searchParams.get("x");
        const paramY = searchParams.get("y");
        const paramZ = searchParams.get("z");
        if (paramX && paramY && paramZ) {
            const x = parseInt(paramX);
            const y = parseInt(paramY);
            const z = parseFloat(paramZ);
            return { x, y, z };
        } else return { x: 0, y: 0, z: 0.02 };
    }

    static async init(div: string) {
        const mapdiv = document.querySelector<HTMLElement>(div);
        if (!mapdiv) console.error("div err");
        else {
            // 16284 , 16802, 3.0739402739427977

            // Create a new application
            const app = new Application();

            // Initialize the application
            await app.init({ resizeTo: mapdiv, antialias: false });

            // Append the application canvas to the document body
            const tl = new MapContainer(app);

            const { x, y, z } = MapContainer.getParams();
            console.log(x, y, z);
            tl.viewport.setZoom(z);
            tl.viewport.moveCorner(x, y);
            tl.updateLayers(x, y, z);

            app.stage.addChild(tl);
            mapdiv.appendChild(app.canvas);
            return app;
        }
    }
}
