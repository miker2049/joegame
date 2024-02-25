function boxSelect(x1, y1, x2, y2) {
    const tl = x1 <= x2 && y1 <= y2 ? { x: x1, y: y1 } : { x: x2, y: y2 };
    const br = x1 <= x2 && y1 <= y2 ? { x: x2, y: y2 } : { x: x1, y: y1 };
    const out = [];
    for (let yy = tl.y; yy < br.y; yy++)
        for (let xx = tl.x; xx < br.x; xx++) out.push([xx, yy]);
    return out;
}
function tsview(name, tsurl, imgurl) {
    if (window.tsviewgame) window.tsviewgame.destroy(true);
    window.tsviewgame = new Phaser.Game({
        render: {
            pixelArt: true,
        },
        type: Phaser.AUTO,
        width: 800,
        height: 600,
        scene: {
            preload: function () {
                this.load.tilemapTiledJSON("tmap", tsurl);
                this.load.image(name, imgurl);
                console.log("hey", name, imgurl, tsurl);
            },
            create: function () {
                console.log(name);
                const map = this.make.tilemap({ key: "tmap" });
                map.addTilesetImage(name, name);
                const lay = map.createLayer("main", name);
                lay.setInteractive({ draggable: true });
                lay.setDepth(3);
                this.input.enabled = true;

                const t = this.add.text(0, 0, "C-c to clear", {
                    color: "red",
                    fontSize: 20,
                });
                t.setScrollFactor(0);
                t.setDepth(2);

                const tilesElem = document.querySelector("#selected-tiles");
                const tilesWidthElem = document.querySelector(
                    "#selected-tiles-width"
                );
                const _highlighted = () =>
                    new Array(map.height)
                        .fill([])
                        .map(() => new Array(map.width).fill(false));
                let highlighted = _highlighted();
                const resetTiles = () => {
                    let selected = [];
                    lay.forEachTile((ti) => {
                        if (highlighted[ti.y][ti.x]) {
                            lay.setTint(0xff0000, ti.x, ti.y);
                            selected.push({
                                x: ti.x,
                                y: ti.y,
                                index: ti.index,
                            });
                        } else lay.setTint(0xffffff, ti.x, ti.y);
                    });
                    if (selected.length === 0) {
                        tilesElem.value = null;
                        tilesWidthElem.value = null;
                    } else {
                        const minX = selected.reduce(
                            (acc, curr) => (curr.x < acc ? curr.x : acc),
                            Infinity
                        );
                        const minY = selected.reduce(
                            (acc, curr) => (curr.y < acc ? curr.y : acc),
                            Infinity
                        );
                        const maxX = selected.reduce(
                            (acc, curr) => (curr.x > acc ? curr.x : acc),
                            0
                        );
                        const maxY = selected.reduce(
                            (acc, curr) => (curr.y > acc ? curr.y : acc),
                            0
                        );
                        const outwidth = maxX - minX + 1;
                        const outheight = maxY - minY + 1;
                        const out = new Array(outheight)
                            .fill([])
                            .map(() => new Array(outwidth).fill(0));
                        selected.forEach(
                            (it) => (out[it.y - minY][it.x - minX] = it.index)
                        );
                        tilesElem.value = out;
                        tilesWidthElem.value = out[0].length;
                    }
                };
                //this.cameras.main.centerOn(map.)
                this.input.on("drag", (pointer, gameObject, dragX, dragY) => {
                    lay.x = dragX;
                    lay.y = dragY;
                    console.log(dragX, dragY);
                    // this.cameras.main.setScroll(
                    //     this.cameras.main.scrollX - dragX,
                    //     this.cameras.main.scrollY - dragY
                    // );
                });
                lay.setScale(
                    Math.min(this.game.config.width, this.game.config.height) /
                        lay.height
                );
                lay.setX(this.game.config.width / 2 - lay.displayWidth / 2);
                lay.setY(lay.y);
                this.input.on("wheel", (pointer, gameObject, dX, dY) => {
                    this.cameras.main.setZoom(
                        this.cameras.main.zoom + dY * 0.001
                    );
                    // lay.setScale(lay.scale + dY * 0.001);
                });
                this.input.keyboard.on("keydown-C", (ev) => {
                    console.log(ev);
                    highlighted = _highlighted();
                    resetTiles();
                });
                this.input.on("pointerup", ({ worldX, worldY, event }) => {
                    const { x: tx, y: ty } = lay.worldToTileXY(worldX, worldY);
                    if (event.shiftKey) console.log(boxSelect(0, 0, tx, ty));
                    if (highlighted[ty] && highlighted[ty][tx] !== undefined) {
                        highlighted[ty][tx] = !highlighted[ty][tx];
                        resetTiles();
                    }
                });
            },
        },
        parent: "tilesetframe",
        dom: {
            createContainer: false,
        },
    });
}
