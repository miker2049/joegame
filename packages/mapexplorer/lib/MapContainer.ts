import { Container, Application, Point, PointData } from "pixi.js";
import { TileLayer } from "./TileLayer";
import { Viewport } from "pixi-viewport";

export class MapContainer extends Container {
    private isDragging = false;
    private lastPosition: PointData | undefined;
    private go: PointData; // global offset
    private gz: number; // global zoom level
    private tls: TileLayer[];
    private minZ = 0;
    private maxZ = 8;
    constructor(
        private app: Application,
        defaultPos?: PointData & { zoom: number },
    ) {
        super();
        this.go = { x: defaultPos?.x || 0, y: defaultPos?.y || 0 };
        this.gz = defaultPos?.zoom || 0;
        //this.setupEventListeners();
        // this.pivot.x = app.screen.width / 2;
        // this.pivot.y = app.screen.height / 2;
        // this.x = app.screen.width / 2;
        // this.y = app.screen.height / 2;

        this.tls = [
            new TileLayer({
                screenWidth: app.screen.width,
                screenHeight: app.screen.height,
                gx: this.go.x,
                gy: this.go.y,
                currZoom: this.gz,
                tileSize: 256,
                zoomLevel: 0,
            }),
            new TileLayer({
                screenWidth: app.screen.width,
                screenHeight: app.screen.height,
                gx: this.go.x,
                gy: this.go.y,
                currZoom: this.gz,
                tileSize: 256,
                zoomLevel: 1,
            }),
            new TileLayer({
                screenWidth: app.screen.width,
                screenHeight: app.screen.height,
                gx: this.go.x,
                gy: this.go.y,
                currZoom: this.gz,
                tileSize: 256,
                zoomLevel: 2,
            }),
            new TileLayer({
                screenWidth: app.screen.width,
                screenHeight: app.screen.height,
                gx: this.go.x,
                gy: this.go.y,
                currZoom: this.gz,
                tileSize: 256,
                zoomLevel: 3,
            }),
            new TileLayer({
                screenWidth: app.screen.width,
                screenHeight: app.screen.height,
                gx: this.go.x,
                gy: this.go.y,
                currZoom: this.gz,
                tileSize: 256,
                zoomLevel: 4,
            }),
            new TileLayer({
                screenWidth: app.screen.width,
                screenHeight: app.screen.height,
                gx: this.go.x,
                gy: this.go.y,
                currZoom: this.gz,
                tileSize: 256,
                zoomLevel: 5,
            }),
        ];
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
        viewport.drag().pinch().wheel().decelerate();
        viewport.setZoom(2);
        viewport.on("moved", ({ viewport }) => {
            const { x, y } = viewport.corner;
            // console.log(x, y);
            this.tls.forEach((tl) => tl.update(x, y, 1));
        });
    }
}
