joegameLib.loadMap({
    objectLayers: ['Background'],
    mapPath: 'assets/maps/library-entrance.json',
    playerStart: {
        x: 533, y: 236
    },
    lights: false
})
    .then(out => {
        var fac = out[1]
        var level = out[0]


        window.fac = fac
        window.level = level

    })
    .catch(err => console.log(err))
