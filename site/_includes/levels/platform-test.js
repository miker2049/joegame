
joegameLib.loadMap({
    objectLayers: ['Emitters', 'aboveObjects', 'swirls', 'items'],
    mapPath: 'assets/maps/platform-level.json',
    playerStart: {
        x: 2032, y: 1712
    },
    // lights: true
})
    .then(out => {
        var fac = out[1]
        var level = out[0]
        window.fac = fac
        window.level = level

    })
    .catch(err => console.log(err))
