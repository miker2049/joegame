# The `joegame` readme

This repository holds `joegame` as it exists as code. Specifically, this is a
typescript project.

The API along with a discussion of internals follows.

## The top level

What is a game? What is a `web app`? Where do we place the game _in_ the app, or
should we do it the other way around?

The joegame repo is fundamentally a number of distinct parts:

- the `src` directory, which contains all the typescript files and is targeted
  by [`esbuild`]( https://esbuild.github.io/) to be transpiled and bundled into
  a small (well, hopefully small) `joegameLib.js` file.  This is the entry point 
  into the library, not yet the entry point to the game.
- the `site` directory contains a simple website structure that is consumed by
  [`eleventy`](https://www.11ty.dev/) in order to create the website.
- the `assets` directory contains assets, which includes a number of different
  folders that are separated by the "type" of the asset. The type of the asset
  is not necessarily it's "format", both images and spritesheets share a format,
  but are of different types. Every `joegameLib` project also expects a
  `data.csv` file at the top of the asset directory. While the asset folder
  contains the files, the `data.csv` file is the real source of truth for
  joegame in general, and includes everything from entries for the actual
  assets, as well as definitions of `objects` and `characters`.
- the `scripts` directory has a collection of bash/nodejs scripts which perform
  simple functions for the repo, this includes the build pipeline config.

The `site` directory is somewhat vulnerable right now to becoming unnecessary in
the future, and while there is much to say about the concept of `assets` in
joegameLib, we will dive right into the real meat here:

### `src`

Whenever I try to understand some computer program I come across, my general
strategy is to try and find where you press "start" and the whole thing boots
up. This is rarely the heuristic used when you _make_ something, to be sure,
where there it is rather more important to consider the functionality itself you
want to address, and what you will need to manifest that functionality. There
are a lot of programming styles, but generally you want to start from the inside
and move outwards, because you never know what your interface(s) are going to
look like by the time you solved the problem, and you should keep yourself
flexible to API changes. But, if I don't really know anything about the program,
and especially if I know very little about the language/framework, I just need
to find the seam. Every program has an `edge` which defines the point where code
leaves itself exposed enough to touch.

If you look at the build process of joegame typescript, its pretty simple, much
thanks to `esbuild` but for other reasons too.

``` javascript 3
// scripts/build-lib-bundle.js
require('esbuild').build({
    entryPoints: [
        './src/index.ts',
    ],
    format: 'iife',
    globalName: 'joegameLib',
    target: require('../browser-targets'),
    bundle: true,
    outfile: 'bundle/joegame-lib.min.js',
    minify: true,
    define: {
        BASEURL: JSON.stringify('/joegame/')
    }
})
```

Ok, so here we are looking at the `build` version of the esbuild config, which
is what would typically be called the "production" build. What it shows is that,
if we did need more evidence than the name of the file, that the entrypoint for
the build is the `index.ts` folder in the src directory.  Ok.

Where does that take us?

``` typescript
import MIDIPlayer from 'timidity-wasm'
import runCinematicNode from './actions/runCinematicNode'
import createTweetConvo from './factories/createTweetConvo'
import joegameFacade from './joegameFacade'
import shaders from './shaders/index'
import { parseCSVRowsToGameData } from "./utils/parseCSVRowsToGameData"
import { parsewikidata as parseOrgWikiData } from "./utils/parseWikiData"
import { loadMap } from './loadMap'
import { happyEmojiReact, sparkleCircle } from 'components/CharEmojiReaction'
import { Menu } from 'components/ui/Menu'

// @ts-ignore
const BASEURL_GLOBAL: string = BASEURL

async function playMIDIFile(path: string, context?: AudioContext) {
    const mplayer = await MIDIPlayer.createMIDIPlayer(BASEURL_GLOBAL,context)
    await mplayer.load(BASEURL_GLOBAL+path)
    return mplayer
}

export {
    joegameFacade,
    runCinematicNode,
    createTweetConvo,
    parseOrgWikiData, //
    parseCSVRowsToGameData,
    shaders,
    playMIDIFile,
    happyEmojiReact,
    sparkleCircle,
    Menu,
    loadMap
}
```

The index file does not hold the app, nor its initialization, but simply exports
an object with a number of functions and classes that are used to make the
joegame happen. This is because we can't know what a given 'joegame' will be,
but rather only know the things joegame will use.

Given this, our next stop on
