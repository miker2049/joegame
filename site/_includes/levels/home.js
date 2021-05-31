/*
 * The home level, where everything happens first
 */
joegameLib.loadLevel(
    'assets/maps/desert_to_home.json',
    '/assets/data.csv',
    'http://localhost:8080/',
    { x: 32, y: 32 })
    .then(lvl => console.log(lvl))
    .catch(err => console.log(err))
