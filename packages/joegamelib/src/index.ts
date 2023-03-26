import { loadLevel } from './main'
fetch('/assets/maps/mmm.json')
  .then((j) => j.json())
  .then((j) => loadLevel(j, 'key'))
  .then((_) => console.log('joegamelib done'))
