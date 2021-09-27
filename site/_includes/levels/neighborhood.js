
joegameLib.loadMap(
    'assets/maps/neighborhood.json',
    '/',
    'assets/data.csv',
    { x: 480, y: 448}, {objectLayers: [] })
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
        const grass = level.map.getLayer('grass')
        console.log(grass)
         level.map.createBlankLayer('overgrass',
                                   grass.tilemapLayer.tileset, 8, 8,
                                   level.map.width, level.map.height,16,16)
        const grassl = level.map.getLayer('overgrass')

        grassl.data = {...grass.data}
        grassl.tilemapLayer.forEachTile(t=>t.setAlpha((Math.random()*0.5)+0.5))
        // grassl.alpha = 0.4

        window.fac = fac
        window.level = level
        window.grassl = grassl
        window.grass = grass
    })
    .catch(err => console.log(err))
