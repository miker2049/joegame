/*
 * The home level, where everything happens first
 */
joegameLib.loadMap(
    'assets/maps/desert_to_home.json',
    '/',
    '/assets/data.csv',
    { x: 64, y: 64 })
    .then(out => {
        var fac = out[1]
        var level = out[0]
        level.player

        level.scene.input.keyboard.on('keydown-SPACE', () => {
            joegameLib.sparkleCircle(level.player)
        })
    })
    .catch(err => console.log(err))
