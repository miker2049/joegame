// import {getMapKeyName, getMapKeyNameRaw, getKeyName, getSceneKeyName} from '../src/levelLoader'
import { getMapKeyNameRaw, chaipromises, expect, parseCSVRowsToWikiData, joegameFacade, testdata } from './imports'
chaipromises()

describe('csv game data parsing', () => {
    it('will fail helpfully if there is no input', () => {
        expect(parseCSVRowsToWikiData("")).to.be.an('object')
    })

    it("parses a csv file ok", () => {
        const obj = parseCSVRowsToWikiData(testdata)
        expect(obj.character).to.be.an('map')
        expect(obj.spritesheet).to.be.an('map')
        expect(obj.mapobject).to.be.an('map')
        expect(obj.image).to.be.an('map')
        expect(obj.platform).to.be.an('map')
    })
    after(() => {
        //cleanup
    })
})

describe('joegame facade', () => {
    let fac = new joegameFacade()
    let game
    describe('initialization', () => {
        it('initializes ok without tweet convos', async () => {
            game = await fac.initGame(parseCSVRowsToWikiData(testdata))
            expect(game).to.be.an.instanceOf(Phaser.Game)
        })
        it('initializes withoutanything', async () => {
            game = await fac.initGame()
            expect(game).to.be.an.instanceOf(Phaser.Game)
        })
    })
    describe('map loading', () => {
        it('loads the test map ok', async () => {
            game = await fac.initGame(parseCSVRowsToWikiData(testdata))
            await fac.loadMapJSON(game, 'assets/maps/testmap.json')
            let mapexists = game.cache.json.exists(getMapKeyNameRaw('assets/maps/testmap.json'))
            expect(mapexists).to.be.true
        })
        it('gives error if no map there', async () => {
            game = await fac.initGame(parseCSVRowsToWikiData(testdata))
            return expect(Promise.resolve(fac.loadMapJSON(game, 'assets/maps/testmapnotthere.json'))).to.be.rejected
        })
    })
    describe('asset loading', () => {
        beforeEach(async () => {
            game = await fac.initGame(parseCSVRowsToWikiData(testdata))
            await fac.loadMapJSON(game, 'assets/maps/testmap.json')
        })
        it('loads stuff from the test map and test data alright', async () => {
            expect(game).to.be.instanceOf(Phaser.Game)

            return expect(Promise.resolve(fac.loadAssets(game, 'assets/maps/testmap.json'))).to.not.be.rejected
        })
        it('loads up expected spritesheets', async () => {
            // const gamee = game as Phaser.Game
            await fac.loadAssets(game, 'assets/maps/testmap.json')
            expect(game.textures.getTextureKeys()).to.be.an('array').and.to.include('studentmale')
            // expect(game.textures.exists("studentmale")).to.be.true
            // expect(gamee.textures.exists("animals2")).to.be.true
            // expect(gamee.textures.exists("scut_extrude-16")).to.be.true
        })
    })
    afterEach(() => {
        if (game) {
            game.destroy(true)
        }
    })
})

