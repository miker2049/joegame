/*
 * The home level, where everything happens first
 */
joegameLib.loadMap({
        objectLayers: ['canyon', 'aboveObjects', 'swirls'],
    playMusic: true,
    runDialogue: true,
    mapPath: 'assets/maps/desert_bridge.json',
    dialogueScript:  'assets/dialogues/swirl-reflection.json',
    musicPath:  'assets/audio/midi/joegame-swirl-slow2.mid'
    })
    .then(out => {
        var fac = out[1]
        var level = out[0]
        document.level = level
    })
    .catch(err => console.log(err))
