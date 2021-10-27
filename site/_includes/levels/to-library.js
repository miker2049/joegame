joegameLib.loadMap({
    objectLayers: ['Emitters', 'Paintings','aboveObjects', 'Overlaps', 'items'],
    mapPath: 'assets/maps/platform-to-library.json',
    playerStart: {
        x: 1586, y: 206
    },
    lights: true
})
    .then(out => {
        var fac = out[1]
        var level = out[0]


        window.fac = fac
        window.level = level

    })
    .catch(err => console.log(err))
