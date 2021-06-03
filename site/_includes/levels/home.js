/*
 * The home level, where everything happens first
 */
joegameLib.loadMap(
    'assets/maps/desert_to_home.json',
    '/',
    '/assets/data.csv',
    { x: 32, y: 32 })
    .then(lvl => console.log(lvl))
    .catch(err => console.log(err))
