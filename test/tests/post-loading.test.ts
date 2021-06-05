
import { InterpreterStatus } from 'xstate'
import { Level } from '../../src/Level'
import { getMapKeyNameRaw, expect, parseCSVRowsToWikiData, joegameFacade, testdataa, TESTMAPPATH } from './imports'

function createFac(): joegameFacade { return new joegameFacade() }
describe('post loading tests', function() {

    this.timeout(4000)
    describe('creating anims, createAnims', function() {
        let game
        let fac
        before(async function() {
            fac = createFac()
            game = await fac.initGame(parseCSVRowsToWikiData(testdataa), '/')
            await fac.loadMapJSON(game, 'assets/maps/testmap.json')
            await fac.loadAssets(game, 'assets/maps/testmap.json')
            fac.createAnims(game)
        })
        it('loads the correct anims from gamedata and spritesheets, for npcs', function() {
            fac.createAnims(game)
            expect(game.anims.exists('animals3_anim_0')).to.be.true
            expect(game.anims.exists('animals3_anim_3')).to.be.true
            expect(game.anims.exists('animals3_anim_6')).to.be.true

            // expect(game.anims.exists('animals2_anim_36')).to.be.true
            expect(game.anims.exists('studentmale_anim_6')).to.be.true
            // game.anims.
        })
        it('loads the correct anims from gamedata and spritesheets, for player', function() {
            expect(game.anims.exists('animals2_anim_36')).to.be.true
        })
        after(() => { game.destroy(true) })
    })
    describe('initiating a joegame-lib level, runLevelScene', function() {
        let game, level, fac

        before(async function() {
            fac = createFac()
            game = await fac.initGame(parseCSVRowsToWikiData(testdataa), '/')
            await fac.loadMapJSON(game, 'assets/maps/testmap.json')
            await fac.loadAssets(game, 'assets/maps/testmap.json')
            fac.createAnims(game)
        })
        it('loads up and returns a Level object', async function() {
            level = fac.runLevelScene(game, TESTMAPPATH)
            expect(level.scene).to.be.an.instanceOf(Phaser.Scene)
        })
        after(() => { game.destroy(true) })
    })
    describe('level work', function() {
        let game: Phaser.Game | undefined, level: Level | undefined, fac: joegameFacade | undefined
        before(async function() {
            // this.timeout(3000)
            fac = createFac()
            game = await fac.initGame(parseCSVRowsToWikiData(testdataa), '/')
            await fac.loadMapJSON(game, 'assets/maps/testmap.json')
            await fac.loadAssets(game, 'assets/maps/testmap.json')
            fac.createAnims(game)
            level = fac.runLevelScene(game, TESTMAPPATH)
        })
        describe('addAllNPCsFromLayer method', function() {
            before(() => {
                fac.addAllNPCsFromLayer(level, 'NPCs')
            })
            it('starts up the right number npcs', function() {
                expect(level.npcs.getTotalUsed()).to.be.eq(2)
            })
            it('starts them up running', async function() {
                expect(level.machineRegistry.checkStatus('Moby')).to.be.eq(InterpreterStatus.Running)
                expect(level.machineRegistry.checkStatus('Maik')).to.be.eq(InterpreterStatus.Running)
            })
        })
        describe('loading up the convo manifest file, given in gdata', function() {
            before(async function() {
                await fac.loadConvoManifestJSON(game)
            })
            it('the manifest files data is available at "convo-manifest"', function() {
                expect(game.cache.json.exists("convo-manifest")).to.be.true
                expect(game.cache.json.get("convo-manifest")).to.be.an('array')
            })
        })
        describe('addAllTweetConvosFromLayer method', function() {
            it('adds tweet convos', async function() {
                const convos = await fac.addAllTweetConvosFromLayer(level, 'TweetConvos')
                expect(convos).to.be.an('array')
            })
        })
        describe(' addAllObjectsFromLayer method', function() {
            it('adds generic objects with correct properties', function() {
                const obj = fac.addAllObjectsFromLayer(level, 'Objects')
                expect(obj.length).to.equal(5)
                // const names = obj.map((item) => item.name);
                // expect(names).to.include('shinyrock')
            })
        })
        describe(' addAllPlatformsFromLayer method', function() {
            it('puts platforms on the map that move', function() {
                expect(fac.addAllPlatformsFromLayer(level, 'Platforms')).to.not.throw
                // testmap shows four objects under Platform
                expect(level.platforms.children.size).to.eq(2)
            })
            it('gives platforms dynamic textures as defined by its key/type', function() {
                const platTexts = level.platforms
                    .getChildren()
                    .map((v: Phaser.GameObjects.Container) => {
                        return (v.list[0] as Phaser.GameObjects.Image).texture.key
                    })
                expect(platTexts).to.include('browserquestextrude')
            })
        })
        describe(' addPlayerToLevel method', function() {
            it('puts the correct player sprite and container and starts necessary machines', function() {
                const player = fac.addPlayerToLevel(level, 44, 75)
                const mach = level.machineRegistry.checkStatus("player_machine")
                level.machineRegistry.startAll()
                expect(player.active).to.be.true
                expect(mach).to.eq(InterpreterStatus.NotStarted)
            })
        })
        describe(' createLevelPhysics method', function() {
            it.skip('how to test this? probably shouldnt need to, should test group membership')
        })
        describe(' createDepthMap method', function() {
            it.skip('puts players, npcs, map  objects, and layers at their correct depth')
        })
        describe(' createTweetConvo method', function() {
            it.skip('create a tweet conversation')
        })
        after(function() { game.destroy(true) })

    })
})
