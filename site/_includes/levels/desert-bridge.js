/*
 * The home level, where everything happens first
 */
joegameLib.loadMap(
    'assets/maps/desert_bridge.json',
    '/',
    '/assets/data.csv',
    { x: 800, y: 748}, {objectLayers: ['canyon'] })
    .then(out => {
        var fac = out[1]
        var level = out[0]
        level.scene.input.keyboard.on('keydown-SPACE', () => {
            joegameLib.sparkleCircle(level.player)
            level.toner.play({ inst: 'walk' })
        })
    })
    .catch(err => console.log(err))
