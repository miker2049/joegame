// import {getMapKeyName, getMapKeyNameRaw, getKeyName, getSceneKeyName} from '../src/levelLoader'
import { getMapKeyNameRaw, chaipromises, expect, parseCSVRowsToWikiData, joegameFacade, testdata } from './imports'
chaipromises()

describe('csv game data parsing, parseCSVRowsToWikiData', () => {
    it.skip('will fail helpfully if there is no input', () => {
        expect(parseCSVRowsToWikiData("")).to.not.be.an('object')
    })

    it("parses a csv file ok", () => {
        const obj = parseCSVRowsToWikiData(testdata)
        const mob = obj.character.get("Moby")
        expect(mob).to.not.be.undefined
        expect(mob.anims.north).to.match(/animals3_anim_84/)
        expect(obj.platform.get('default').texture).to.match(/scut_ext/)
        expect(obj.platform.get('cobblestone').texture).to.match(/browserquestext/)
        expect(obj.platform.get('cobblestone').groundTiles).to.include(414).and.include(415)
        expect(obj.platform.get('cobblestone').edgeTiles).to.include(475)
        expect(obj.character.get("Maik")).to.not.be.undefined
    })
    after(() => {
        //cleanup
    })
})

describe('joegame facade', () => {
    let fac = new joegameFacade()
    let game
    describe('initialization, initGame', () => {
        it('initializes ok without tweet convos', async () => {
            game = await fac.initGame(parseCSVRowsToWikiData(testdata))
            expect(game).to.be.an.instanceOf(Phaser.Game)
        })
        it('initializes withoutanything', async () => {
            game = await fac.initGame()
            expect(game).to.be.an.instanceOf(Phaser.Game)
        })
        afterEach(() => { game.destroy(true) })
    })
    describe('map loading, loadMapJSON', () => {
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
        afterEach(() => { game.destroy(true) })
    })
    describe('asset loading from mapjson and csv, loadAssets', () => {
        before(async () => {
            game = await fac.initGame(parseCSVRowsToWikiData(testdata))
            await fac.loadMapJSON(game, 'assets/maps/testmap.json')
            await fac.loadAssets(game, 'assets/maps/testmap.json')
        })
        it('loads up expected spritesheets from NPC layer', async () => {
            expect(game.textures.getTextureKeys()).to.be.an('array')
                .and.to.include('studentmale')
                .and.to.include('animals3')
        })
        it('loads up tileset images from regular tile layers', () => {
            expect(game.textures.exists("scut_extrude-16")).to.be.true
        })
        it.skip('loads up tileset images from "Player" tile layers', () => {
            expect(game.textures.exists("animals2")).to.be.true
        })
        it('loads up images from image collection tilesets', () => {
            // expect(game.textures.exists("canyon")).to.be.true
            // console.log(game.textures.getTextureKeys())
            // expect((game as Phaser.Game).scene.getAt(0).load.)
        })
        it.skip('loads up images from platforms correctly ', () => {
            expect(game.textures.exists("animals2")).to.be.true
        })
        it.skip('loads up images and reqs for generic mapobjects correctly', () => {
            expect(game.textures.exists("animals2")).to.be.true
        })
        it.skip('loads up midi files from manifest correctly', () => {
            expect(game.textures.exists("animals2")).to.be.true
        })
        it.skip('loads up convo manifest file manifest correctly', () => {
            expect(game.textures.exists("animals2")).to.be.true
        })
        it.skip('loads up book manifest file manifest correctly', () => {
            expect(game.textures.exists("animals2")).to.be.true
        })
        after(() => { game.destroy(true) })
    })
    describe('post loading tests', () => {
        before(async () => {
            game = await fac.initGame(parseCSVRowsToWikiData(testdata))
            await fac.loadMapJSON(game, 'assets/maps/testmap.json')
            await fac.loadAssets(game, 'assets/maps/testmap.json')
            fac.createAnims(game)
        })
        describe('creating anims, createAnims', () => {
            it('loads the correct anims from gamedata and spritesheets, for npcs', () => {
                fac.createAnims(game)
                expect(game.anims.exists('animals3_anim_0')).to.be.true
                expect(game.anims.exists('animals3_anim_3')).to.be.true
                expect(game.anims.exists('animals3_anim_6')).to.be.true

                // expect(game.anims.exists('animals2_anim_36')).to.be.true
                expect(game.anims.exists('studentmale_anim_6')).to.be.true
                // game.anims.
            })
            it('loads the correct anims from gamedata and spritesheets, for player', () => {
                expect(game.anims.exists('animals2_anim_36')).to.be.true
            })
            describe('initiating a joegame-lib level, runLevelScene', () => {
                it.skip('loads up and returns a Level object')
            })
            describe('addAllNPCsFromLayer method', () => {
                it.skip('starts up npcs')
            })
            describe('addAllTweetConvosFromLayer method', () => {
                it.skip('adds tweet convos')
            })
            describe(' addAllObjectsFromLayer method', () => {
                it.skip('adds generic objects with correct properties')
            })
            describe(' addAllPlatformsFromLayer method', () => {
                it.skip('puts platforms on the map the move')
                it.skip('gives platforms dynamic textures as defined by its key/type')
            })
            describe(' addPlayerToLevel method', () => {
                it.skip('puts the correct player sprite and container and starts necessary machines')
            })
            describe(' createLevelPhysics method', () => {
                it.skip('how to test this? probably shouldnt need to, should test group membership')
            })
            describe(' createDepthMap method', () => {
                it.skip('puts players, npcs, map  objects, and layers at their correct depth')
            })
            describe(' createTweetConvo method', () => {
                it.skip('create a tweet conversation')
            })
            after(() => { game.destroy(true) })
        })
    })
})


