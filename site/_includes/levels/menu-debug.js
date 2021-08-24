
joegameLib.loadMap(
    'assets/maps/empty.json',
    '/',
    'assets/data.csv',
    { x: -1, y: -1}, {})
    .then(out => {
        const level = out[0]
        let m=joegameLib.createMenu(level.scene.game.scene)

        m.open()
        window.m = m
    })
    .catch(err => console.log(err))
