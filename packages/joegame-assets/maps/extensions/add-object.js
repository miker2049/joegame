function addObject() {
    const tiles = tiled.activeAsset.selectedTiles.map((it) => it.id);
    const image = tiled.activeAsset.image;
    const name = tiled.prompt("What is the name of this object?");
    const width = tiled.prompt("The width?");
    const proc = new Process();
    proc.exec("nix", [
        "run",
        "./joegame#emacss",
        "--",
        "--script",
        "joegame/assets/add-mapobject.el", //Tiled is at ~
        name,
        FileInfo.baseName(image),
        tiles,
        width,
    ]);
}

const act = tiled.registerAction("add-mapobject", () => {
    addObject();
    tiled.log("done");
});

act.shortcut = "Ctrl+L";
act.text = "Add a Mapobject to data";
tiled.extendMenu("Edit", [
    { action: "add-mapobject", before: "align-object" },
    { separator: true },
]);

// addCharacter
function addCharacter() {
    const tiles = tiled.activeAsset.selectedTiles.map((it) => it.id);
    const south = tiles.slice(0, 3);
    const west = tiles.slice(3, 6);
    const east = tiles.slice(6, 9);
    const north = tiles.slice(9, 12);

    const image = tiled.activeAsset.image;
    const name = tiled.prompt("What is the name of this character?");
    const proc = new Process();
    proc.exec("nix", [
        "run",
        "./joegame#emacss",
        "--",
        "--script",
        "joegame/assets/add-character.el", //Tiled is at ~
        name,
        FileInfo.baseName(image),
        north,
        south,
        east,
        west,
    ]);
}

const cact = tiled.registerAction("add-character", () => {
    addCharacter();
    tiled.log("done");
});

cact.shortcut = "Ctrl+Shift+L";
cact.text = "Add a Character to data";
tiled.extendMenu("Edit", [
    { action: "add-character", before: "add-mapobject" },
    { separator: true },
]);
