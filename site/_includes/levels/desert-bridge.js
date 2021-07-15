/*
 * The home level, where everything happens first
 */
joegameLib.loadMap(
    'assets/maps/desert_bridge.json',
    '/',
    'assets/data.csv',
    { x: 480, y: 448}, {objectLayers: ['canyon', 'aboveObjects', 'swirls'] })
    .then(out => {
        var fac = out[1]
        var level = out[0]
        var context = level.scene.sound.context
        joegameLib.playMIDIFile('assets/audio/midi/joegame-swirl-slow2.mid', context).then(plyr => {
            console.log(plyr)

            console.log('***********************************8')
            level.scene.input.keyboard.on('keydown-SPACE', () => {
                joegameLib.sparkleCircle(level.player)
                plyr.play()
            })
        })
        joegameLib.runCinematicNode(level, 'Start', 'assets/dialogues/swirl-reflection.json')
    })
    .catch(err => console.log(err))
