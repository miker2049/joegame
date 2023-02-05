function addObject() {
    const tiles = tiled.activeAsset.selectedTiles.map((it) => it.id);
    const image = tiled.activeAsset.image;
    const name = tiled.prompt("What is the name of this object?");
    const width = tiled.prompt("The width?");
    const proc = new Process();
    proc.exec("emacs", [
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
