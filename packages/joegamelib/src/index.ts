import { loadLevel } from './main'

console.log('conf_test')
fetch('/assets/maps/tile_server_test.json')
  .then((j) => j.json())
  .then((j) => loadLevel(j, 'key'))
  .then((_) => console.log('joegamelib done'))
