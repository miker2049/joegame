export class ObjectPool<
    T extends new (...args: ConstructorParameters<T>) => V,
    V = InstanceType<T>,
> {
    private pool: V[] = [];
    private inUse: Set<V> = new Set();

    constructor(
        size: number,
        private thisclass: T,
        private defaultArgs: ConstructorParameters<T>,
    ) {
        for (let i = 0; i < size; i++) {
            this.pool.push(new thisclass(...this.defaultArgs));
        }
    }

    get(): V {
        let sprite = this.pool.pop();
        if (!sprite) {
            sprite = new this.thisclass(...this.defaultArgs);
        }
        this.inUse.add(sprite);
        return sprite;
    }

    release(sprite: V) {
        this.inUse.delete(sprite);
        this.pool.push(sprite);
    }
}

export interface Rect {
    x: number;
    y: number;
    w: number;
    h: number;
}
/**
 * Return a list of absolute coordinates that are overlapped in the new rect
 */
export function getOverlappingTiles(
    rect1: Rect,
    rect2: Rect,
): [number, number][] {
    const overlapTiles: [number, number][] = [];

    // Find the intersection rectangle
    const left = Math.max(rect1.x, rect2.x);
    const top = Math.max(rect1.y, rect2.y);
    const right = Math.min(rect1.x + rect1.w, rect2.x + rect2.w);
    const bottom = Math.min(rect1.y + rect1.h, rect2.y + rect2.h);

    // If there's no overlap, return an empty array
    if (left >= right || top >= bottom) {
        return overlapTiles;
    }

    // Iterate through the overlapping area
    for (let x = left; x < right; x++) {
        for (let y = top; y < bottom; y++) {
            overlapTiles.push([x, y]);
        }
    }

    return overlapTiles;
}

export function getClosestMultiple(n: number, multiple: number): number {
    const remainder = n % multiple;

    if (remainder === 0) {
        return n; // n is already a multiple of 'multiple'
    }

    const lowerMultiple = n - remainder;
    const upperMultiple = lowerMultiple + multiple;

    // Compare which multiple is closer
    return n - lowerMultiple < upperMultiple - n
        ? lowerMultiple
        : upperMultiple;
}

export function doRectsIntersect(rect1: Rect, rect2: Rect): boolean {
    return (
        rect1.x < rect2.x + rect2.w &&
        rect1.x + rect1.w > rect2.x &&
        rect1.y < rect2.y + rect2.h &&
        rect1.y + rect1.h > rect2.y
    );
}
export function getRectTiles(obj: Rect): [number, number][] {
    const { x, y, w, h } = obj;
    return Array(h)
        .fill(0)
        .map((_, yidx) =>
            Array(w)
                .fill(0)
                .map((_, xidx) => [xidx + x, yidx + y] as [number, number]),
        )
        .flat();
}
