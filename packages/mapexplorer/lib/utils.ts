import { Texture, Assets } from "pixi.js";
import { Pnt } from "./types";
import { JTilemap } from "./JTilemap";
import jdb from "./jdb.json";
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

// pairing function
// look here for maybe better one: http://szudzik.com/ElegantPairing.pdf (https://stackoverflow.com/questions/919612/mapping-two-integers-to-one-in-a-unique-and-deterministic-way)
export function cantor(a: number, b: number) {
    return ((a + b + 1) * (a + b)) / 2 + b;
}

export function invertCantor(z: number): Pnt {
    const w = Math.floor((Math.sqrt(8 * z + 1) - 1) / 2);
    const t = (w ** 2 + w) / 2;
    const y = z - t;
    const x = w - y;
    return [x, y];
}

export function hashint(a: number, b = 0, c = 0) {
    return cantor(a, cantor(b, c));
}

// note: used for tilemap cache so the actually used ranges are 0-255,0-255,0-7,0-7.
// Testing all combinations,
export function hashint4(a: number, b: number, c: number, d: number) {
    return cantor(a, cantor(b, cantor(c, d)));
}

export function invertHashint(z: number) {
    const [a, bc] = invertCantor(z);
    const [b, c] = invertCantor(bc);
    return [a, b, c];
}

export function invertHashint4(z: number) {
    const [a, bcd] = invertCantor(z);
    const [b, cd] = invertCantor(bcd);
    const [c, d] = invertCantor(cd);
    return [a, b, c, d];
}

class LRUCache<K = number, V = number> {
    capacity: number;
    cache: Map<K, V>;
    constructor(capacity: number) {
        this.capacity = capacity;
        this.cache = new Map<K, V>();
    }

    get(key: K): V | undefined {
        if (!this.cache.has(key)) return undefined;

        // Remove the entry and re-insert it to put it at the end (most recently used)
        const value = this.cache.get(key);
        this.cache.delete(key);
        this.cache.set(key, value as V);
        return value;
    }

    put(key: K, value: V) {
        if (this.cache.has(key)) {
            // If the key exists, remove it so we can put it at the end
            this.cache.delete(key);
        } else if (this.cache.size >= this.capacity) {
            // If we're at capacity, remove the least recently used item
            const leastUsedKey = this.cache.keys().next().value;
            this.cache.delete(leastUsedKey);
        }

        // Add the new item to the end of the Map
        this.cache.set(key, value);
    }
}

export class TileCache {
    cache: LRUCache<number, Texture>;
    constructor(capacity: number) {
        this.cache = new LRUCache<number, Texture>(capacity);
    }
    async getTile(
        x: number,
        y: number,
        z: number,
    ): Promise<[Texture, [number, number, number]]> {
        const hash = hashint(x, y, z);
        const cacheVal = this.cache.get(hash);
        if (cacheVal) return [cacheVal, [x, y, z]];
        else {
            const t = await this.fetchTile(x, y, z);
            t.source.scaleMode = "nearest";
            this.cache.put(hash, t);
            return [t, [x, y, z]];
        }
    }
    private fetchTile(x: number, y: number, z: number) {
        return Assets.load({
            loadParser: "loadTextures", // will force it to be handled as a texture
            src: `http://localhost:5000/worldtile/${z}/${x}/${y}`,
        });
    }
}

export class TilemapCache {
    cache: LRUCache<number, Texture>;
    constructor(capacity: number) {
        this.cache = new LRUCache<number, Texture>(capacity);
    }
    async getMap(
        x: number,
        y: number,
        file: number,
        rank: number,
    ): Promise<[JTilemap, [number, number, number, number]]> {
        const hash = hashint4(x, y, file, rank);
        const cacheVal = this.cache.get(hash);
        if (cacheVal) return [cacheVal, [x, y, z]];
        else {
            const t = await this.fetchTile(x, y, z);
            t.source.scaleMode = "nearest";
            this.cache.put(hash, t);
            return [t, [x, y, z]];
        }
    }
    private fetchTile(x: number, y: number, z: number) {
        return Assets.load({
            loadParser: "loadTextures", // will force it to be handled as a texture
            src: `http://localhost:5000/worldtile/${z}/${x}/${y}`,
        });
    }
}

export async function loadPixelAsset(
    alias: string,
    src: string,
    scaleMode = "nearest",
    overwrite = false,
) {
    if (Assets.get(alias) && !overwrite) return;

    Assets.add({
        alias,
        src,
        data: { scaleMode },
    });
    return await Assets.load(alias);
}

export function string2hex(str: string) {
    return str.split("").map((it) => parseInt(it, 16));
}

export function getSearchParams(
    search = window.location.search,
): URLSearchParams {
    const searchParams = new Proxy(new URLSearchParams(search), {
        get: (params, prop: string) => params.get(prop),
    });
    return searchParams;
}

export function getUnique<T>(ls: T[]): T[] {
    return Array.from(new Set(ls));
}

function getObjInfo(name: string) {
    const obj = jdb.mapobjects[name];
    obj.assets = obj.req_image.map((im) => getAssetInfo(im));
    return obj;
}
function getAssetInfo(name: string) {
    return jdb.images[name];
}

export async function makeObjectLayers(objects: [string, number, number][]) {
    const objNames = getUnique(objects.map((it) => it[0]));
    const objInfo = objNames.map((it) => getObjInfo(it));
    console.log(objInfo);
}
