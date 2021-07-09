/*
 * The home level, where everything happens first
 */
joegameLib.loadMap(
    'assets/maps/desert_bridge.json',
    '/',
    '/assets/data.csv',
    { x: 480, y: 448}, {objectLayers: ['canyon', 'aboveObjects', 'swirls'] })
    .then(out => {
        var fac = out[1]
        var level = out[0]
        joegameLib.runCinematicNode(level,'Start','assets/dialogues/swirl-reflection.json')
        level.scene.input.keyboard.on('keydown-SPACE', () => {
            joegameLib.sparkleCircle(level.player)
        })
    })
    .catch(err => console.log(err))
