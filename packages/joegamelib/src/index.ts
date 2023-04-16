import { loadLevel } from './main'

// import { inspect } from '@xstate/inspect'

// inspect({ iframe: false })
fetch('/assets/maps/mmm.json')
  .then((j) => j.json())
  .then((j) => loadLevel(j, 'key'))
  .then((_) => console.log('joegamelib done'))
