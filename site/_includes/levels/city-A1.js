
joegameLib.loadMap(
    'assets/maps/cityA1.json',
    '/',
    'assets/data.csv',
    { x: 22*16, y: 22*16}, {objectLayers: ['background'] })
    .then(out => {
        var fac = out[1]
        var level = out[0]
        // var context = level.scene.sound.context
        // joegameLib.playMIDIFile('assets/audio/midi/joegame-swirl-slow2.mid', context).then(plyr => {
        //     console.log(plyr)

        //     console.log('***********************************8')
        //     level.scene.input.keyboard.on('keydown-SPACE', () => {
        //         joegameLib.sparkleCircle(level.player)
        //         plyr.play()
        //     })
        // })
        // joegameLib.runCinematicNode(level, 'Start', 'assets/dialogues/swirl-reflection.json')
        window.fac = fac
        window.level = level
    })
    .catch(err => console.log(err))
