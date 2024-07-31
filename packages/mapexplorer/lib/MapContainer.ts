import { Container, Application, Point, PointData } from "pixi.js";

export class MapContainer extends Container {
    private isDragging = false;
    private lastPosition: PointData | undefined;
    private go: PointData; // global offset
    private gz: number; // global zoom level
    constructor(
        private app: Application,
        child: Container,
        defaultPos: PointData & { zoom: number },
    ) {
        super();
        this.go = { x: defaultPos.x || 0, y: defaultPos.y || 0 };
        this.gz = defaultPos.zoom || 0;
        this.setupEventListeners();
        this.addChild(child);
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
        //  this.addEventListener('wheel', this.onZoom.bind(this));
    }

    private onDragStart(event: MouseEvent): void {
        this.isDragging = true;
        this.lastPosition = new Point(event.clientX, event.clientY);
    }

    private onDragMove(event: MouseEvent): void {
        if (!this.isDragging || !this.lastPosition) return;

        const newPosition = new Point(event.clientX, event.clientY);
        const dx = newPosition.x - this.lastPosition.x;
        const dy = newPosition.y - this.lastPosition.y;
        const sc = (1 / 256) * Math.pow(2, this.gz);
        this.go = { x: this.go.x + dx * sc, y: this.go.y + dy * sc };
        //this.y += dy;

        this.lastPosition = newPosition;
    }

    private onDragEnd(): void {
        this.isDragging = false;
        this.lastPosition = undefined;
    }
}
