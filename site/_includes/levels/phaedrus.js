
joegameLib.loadMap({
    playerStart: { x: 480, y: 448 },
    objectLayers: ['Emitters', 'aboveObjects', 'swirls', 'items'],
    runDialogue: true,
    mapPath: 'assets/maps/small-garden-phaedrus.json',
    dialogueScript: 'assets/dialogues/phaedrus_dialogue.txt',
    dialogueScriptFormat: 'text'
})
    .then(out => {
        var fac = out[1]
        var level = out[0]
        window.fac = fac
        window.level = level

    })
    .catch(err => console.log(err))
