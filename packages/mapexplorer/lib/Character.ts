import { js as Easystar } from "easystarjs";
import {
    AnimatedSprite,
    Application,
    Container,
    Spritesheet,
    SpritesheetData,
    Ticker,
} from "pixi.js";
import { jdb } from "./jdb";
import { asyncTimeout, loadPixelAsset } from "./utils";
import { Tween } from "@tweenjs/tween.js";
import { JTilemap } from "./JTilemap";
import { throttle } from "underscore";
import { Direction } from "./types";

export type AnimatedSpriteConfig = {
    terr: string;
    spr: {
        [key: string]: AnimatedSprite;
    };
};
export async function createAnimatedSprites(
    char: keyof typeof jdb.characters,
    terr: string,
): Promise<AnimatedSpriteConfig> {
    const atlas = parseConfigToAtlas(char);
    if (!atlas.meta.image) throw Error("Cant find image prop for " + char);
    const texture = await loadPixelAsset(atlas.meta.image, atlas.meta.image);
    const sheet = new Spritesheet(texture, atlas);
    await sheet.parse();
    if (!sheet.animations) throw Error("No anims for " + char);
    return {
        terr,
        spr: Object.fromEntries(
            Object.entries(sheet.animations).map(([key, anim]) => [
                key,
                new AnimatedSprite(anim),
            ]),
        ),
    };
}
function parseConfigToAtlas<T extends keyof typeof jdb.characters>(
    char: T,
): SpritesheetData {
    const conf = jdb.characters[char];
    const texture = jdb.images[conf.texture];
    if (!texture.frameConfig) throw Error("Issue with spriteshee " + char);

    const { frameWidth, frameHeight, tilecount } = texture.frameConfig;
    const tileWidth = texture.frameConfig.imagewidth / frameWidth;
    const entries = Array(tilecount)
        .fill(0)
        .map((_, idx) => [
            `${conf.name}_${idx}`,
            {
                frame: {
                    x: (idx % tileWidth) * frameWidth,
                    y: Math.floor(idx / tileWidth) * frameHeight,
                    w: frameWidth,
                    h: frameHeight,
                },
            },
        ]);
    const frames = Object.fromEntries(entries);

    const animEntries = Object.entries(conf.anims).map(([key, anim]) => [
        key,
        anim.map((a) => `${conf.name}_${a}`),
    ]);

    return {
        animations: Object.fromEntries(animEntries),
        frames,
        meta: {
            image: `http://localhost:5000/${texture.url}`,
            scale: 1,
        },
    };
}

export class Character extends Container {
    velMod = 10;
    tw: Tween | undefined;
    private lastPos: { x: number; y: number };
    private currDir: Direction;
    private setDirectionThrot: () => void;
    animSprites: AnimatedSpriteConfig["spr"];
    terrain: string;

    constructor(
        sprConfig: AnimatedSpriteConfig,
        private app: Application,
    ) {
        super();
        this.terrain = sprConfig.terr;
        this.animSprites = sprConfig.spr;
        this.setAnimSprite(Direction.south);
        this.app.ticker.add(this.update.bind(this));
        this.sprite.animationSpeed = 1 / 60;
        this.position.x = Math.random() * 500;
        this.position.y = Math.random() * 500;
        this.lastPos = { x: this.position.x, y: this.position.y };
        this.currDir = Direction.south;
        this.setDirectionThrot = throttle(this.setDirection.bind(this), 133);
        this.sprite.play();
    }

    setDirection() {
        const dx = this.position.x - this.lastPos.x;
        const dy = this.position.y - this.lastPos.y;

        const direction =
            dx === 0 && dy === 0
                ? undefined
                : Math.abs(dx) > Math.abs(dy)
                  ? dx > 0
                      ? Direction.east
                      : Direction.west
                  : dy > 0
                    ? Direction.south
                    : Direction.north;

        if (direction) this.setAnimSprite(direction);

        this.lastPos.x = this.position.x;
        this.lastPos.y = this.position.y;
    }

    setSprite(sprite: AnimatedSprite) {
        sprite.animationSpeed = 1 / 8;
        sprite.anchor = { x: 0.5, y: 1 };
        this.removeChildren();
        this.addChild(sprite);
    }
    setAnimSprite(k: Direction) {
        if (this.currDir !== k) {
            this.setSprite(this.animSprites[k]);
            this.currDir = k;
            this.sprite.play();
        }
    }

    get sprite(): AnimatedSprite {
        return this.children[0] as AnimatedSprite;
    }

    update(_d: Ticker) {
        this.tw?.update();
        this.setDirectionThrot();
    }

    move({ x, y }: { x?: number; y?: number }) {
        return new Promise((res, _rej) => {
            const toObj: { x?: number; y?: number } = {};
            toObj.x = x ? Math.max(0, this.position.x + x) : this.position.x;
            toObj.y = y ? Math.max(0, this.position.y + y) : this.position.y;

            this.tw = new Tween({ x: this.position.x, y: this.position.y })
                // .to(toObj, (x || y || 1) * this.velMod)
                .to(toObj, Math.abs((x || y || 1) * this.velMod))
                .onUpdate(({ x: xx, y: yy }) => {
                    this.position.x = xx;
                    this.position.y = yy;
                })
                .onComplete(() => {
                    res(null);
                    this.sprite.stop();
                })
                .startFromCurrentValues();

            // if (x && x > 0) this.setAnimSprite("east");
            // else if (x && x < 0) this.setAnimSprite("west");
            // else if (y && y > 0) this.setAnimSprite("south");
            // else this.setAnimSprite("north");
            // this.sprite.play();
        });
    }
}

/** Pathfinding wanderer */
export class Wanderer extends Character {
    active = true;
    pathfinder: Easystar;
    constructor(
        animSprites: AnimatedSpriteConfig,
        app: Application,
        private parentJtilemap: JTilemap,
    ) {
        super(animSprites, app);
        this.randMove();
        this.pathfinder = this.parentJtilemap.pathfinder;
    }

    getTilePos(): [number, number] {
        const ts = this.parentJtilemap.layers[0].tileSize;
        return [
            Math.floor(this.position.x / ts),
            Math.floor(this.position.y / ts),
        ];
    }

    async findPath(x: number, y: number): Promise<{ x: number; y: number }[]> {
        // debugger;
        const pathP = new Promise<{ x: number; y: number }[]>((res) =>
            this.pathfinder.findPath(...this.getTilePos(), x, y, (path) =>
                res(path),
            ),
        );
        this.pathfinder.calculate();
        return pathP;
    }

    async randMove() {
        if (Math.random() > 0.5) {
            await this.move({ x: Math.random() * 200 - 100 });
        } else {
            await this.move({ y: Math.random() * 200 - 100 });
        }
        await asyncTimeout(1000 + Math.random() * 600);
        if (this.active) this.randMove();
    }
    movePath(path: { x: number; y: number }[]) {
        this.tw?.stop();
        return new Promise((res, _rej) => {
            const toObj: { x: number[]; y: number[] } = path.reduce(
                (
                    acc: { x: number[]; y: number[] },
                    curr: { x: number; y: number },
                ) => {
                    acc.x.push(curr.x * this.parentJtilemap.layers[0].tileSize);
                    acc.y.push(curr.y * this.parentJtilemap.layers[0].tileSize);
                    return acc;
                },
                { x: [], y: [] },
            );
            console.log(toObj);
            this.sprite.play();
            this.tw = new Tween({ x: this.position.x, y: this.position.y })
                // .to(toObj, (x || y || 1) * this.velMod)
                .to(toObj, Math.abs(16 * path.length * this.velMod))
                .onUpdate(({ x: xx, y: yy }) => {
                    this.position.x = xx;
                    this.position.y = yy;
                })
                .onComplete(() => {
                    res(null);
                    this.sprite.stop();
                })
                .start();

            // if (x && x > 0) this.setAnimSprite("east");
            // else if (x && x < 0) this.setAnimSprite("west");
            // else if (y && y > 0) this.setAnimSprite("south");
            // else this.setAnimSprite("north");
            // this.sprite.play();
        });
    }
}
