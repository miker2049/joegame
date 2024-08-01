import { Container, Application, Point, PointData } from "pixi.js";
import { TileLayer } from "./TileLayer";
import { Viewport } from 'pixi-viewport'

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
        this.setupEventListeners();
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
        ];
        this.tls.forEach((tl) => this.addChild(tl));
    }



    private setupEventListeners(): void {
        this.app.canvas.addEventListener(
            "mousedown",
            this.onDragStart.bind(this),
        );
        this.app.canvas.addEventListener(
            "mousemove",
            this.onDragMove.bind(this),
        );
        this.app.canvas.addEventListener("mouseup", this.onDragEnd.bind(this));
        this.app.canvas.addEventListener("wheel", this.onWheel.bind(this));
    }

    private onDragStart(event: MouseEvent): void {
        this.isDragging = true;
        this.lastPosition = new Point(event.clientX, event.clientY);
    }

    private onDragMove(ev: MouseEvent): void {
        if (!this.isDragging || !this.lastPosition) return;

        const newPosition = new Point(ev.clientX, ev.clientY);
        const dx = ev.clientX - this.lastPosition.x;
        const dy = ev.clientY - this.lastPosition.y;
        const sc = (1 / 256) * 2 ** this.gz;
        this.go = { x: this.go.x + dx * sc, y: this.go.y + dy * sc };
        // console.log(this.go);
        //this.y += dy;

        this.lastPosition = newPosition;
        this.updateTileLayers();
    }
    private onWheel(event: WheelEvent): void {
        const oldGz = this.gz;
        console.log(this.gz);
        if (event.deltaY > 0) this.gz = Math.max(this.gz - 0.025, this.minZ);
        else this.gz = Math.min(this.maxZ, this.gz + 0.025);
        const scaleChange = this.gz - oldGz;
        const ox = -(event.clientX * scaleChange);
        const oy = -(event.clientY * scaleChange);
        //console.log(this.gz);
        const sc = (1 / 256) * 2 ** this.gz;
        this.go = { x: this.go.x + ox * sc, y: this.go.y + oy * sc };
        this.updateTileLayers();
    }

    private updateTileLayers() {
        //console.log(this.go.x, this.go.y, this.gz);
        this.tls.forEach((tl) => tl.update(this.go.x, this.go.y, this.gz));
        console.log("-------");
    }

    private onDragEnd(): void {
        this.isDragging = false;
        this.lastPosition = undefined;
    }
}
