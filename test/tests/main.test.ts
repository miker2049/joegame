import {
    getMapKeyNameRaw,
    expect,
    parseCSVRowsToGameData,
    joegameFacade as fac,
    testdataa,
} from "./imports";
import gameconfig from 'joegamelib/src/gameconfig'
import chaiAsPromised from "chai-as-promised";
const TESTMAPPATH = "assets/maps/testmap.json";
const BASEURL = "/";
chai.use(chaiAsPromised)
// mocha.che();
describe("csv game data parsing, parseCSVRowsToWikiData", () => {
    it.skip("will fail helpfully if there is no input", () => {
        expect(parseCSVRowsToGameData("")).to.not.be.an("object");
    });

    it("parses a csv file ok", () => {
        console.log(testdataa)
        const obj = parseCSVRowsToGameData(testdataa);
        const mob = obj.character.get("Moby");
        expect(mob).to.not.be.undefined;
        expect(mob!.anims.north).to.match(/animals3_anim_84/);
        expect(obj.platform.get("default")!.texture).to.match(/scut_ext/);
        expect(obj.platform.get("cobblestone")!.texture).to.match(
            /browserquestext/
        );
        expect(obj.platform.get("cobblestone")!.groundTiles)
            .to.include(414)
            .and.include(415);
        expect(obj.platform.get("cobblestone")!.edgeTiles).to.include(475);
        expect(obj.mapobject.get("shinyrock")!.req_image).to.include(
            "shinyrock1"
        );
        expect(obj.character.get("Maik")).to.not.be.undefined;
        expect(obj.sound.get("walk")).to.not.be.undefined;
        expect(obj.sound.get("vowel")).to.not.be.undefined;
        // expect(obj.sound.get("vowel").splitLength).to.eq(1000);
        // expect(obj.convoManifest).to.match(/assets\/tweet\-convos\/convo-manifest.json/);
    });
    after(() => {
        //cleanup
    });
});

describe("joegame facade", () => {

    const dconf = { mapPath: TESTMAPPATH,
                    platformLayers: ["Platforms"],
                    objectLayers: ["Objects"],
                  }
    describe("initialization, initGame", () => {
        let game;
        it("initializes ok without tweet convos", async () => {
            game = await fac.initGame('/', gameconfig)
            expect(game).to.be.an.instanceOf(Phaser.Game);
        });
        it.skip("initializes withoutanything", async () => {
            game = await fac.initGame();
            expect(game).to.be.an.instanceOf(Phaser.Game);
        });
        afterEach(() => {
            game.destroy(true);
        });
    });
    describe("map loading, loadMapJSON", () => {
        let game;
        it("loads the test map ok", async () => {
            game = await fac.initGame('/', gameconfig)
            await fac.loadMapJSON(game, dconf);
            let mapexists = game.cache.json.exists(
                getMapKeyNameRaw("assets/maps/testmap.json")
            );
            expect(mapexists).to.be.true;
        });
        it("gives error if no map there", async () => {
            game = await fac.initGame('/', gameconfig)
            return expect(
                Promise.resolve(
                    fac.loadMapJSON(game, "assets/maps/testmapnotthere.json")
                )
            ).to.be.rejected;
        });
        afterEach(() => {
            game.destroy(true);
        });
    });
    describe("asset loading from mapjson and csv, loadAssets", function () {
        let game;
        before(async function () {
            game = await fac.initGame('/', gameconfig)
            await fac.loadMapJSON(game, dconf);
            await fac.loadAssets(game, dconf).catch((err) => {
                console.dir(err);
                throw new Error(err);
            });
        });
        it("has a loaded up gdata", function () {
            expect(game.cache.json.get("gdata")).to.be.an("object");
        });
        it("loads up expected spritesheets from NPC layer", async function () {
            expect(game.textures.getTextureKeys())
                .to.be.an("array")
                .and.to.include("studentmale")
                .and.to.include("animals3");
        });
        it("loads up tileset images from regular tile layers", function () {
            expect(game.textures.exists("scut_extrude-16")).to.be.true;
        });
        it('loads up tileset images from "Player" tile layers', function () {
            expect(game.textures.exists("animals2")).to.be.true;
        });
        it("loads up images from image collection tilesets", function () {
            expect(game.textures.exists("121")).to.be.true;
            expect(game.textures.exists("122")).to.be.true;
            // expect((game as Phaser.Game).scene.getAt(0).load.)
        });
        it("loads up images from platforms correctly ", function () {
            expect(game.textures.exists("browserquestextrude")).to.be.true;
        });
        it.skip("loads up images and reqs for generic mapobjects correctly", function () {
            expect(game.textures.exists("shinyrock1")).to.be.true;
            expect(game.textures.exists("shinyrock2")).to.be.true;
        });
        it.skip("loads up midi files from manifest correctly", () => {
            expect(game.textures.exists("animals2")).to.be.true;
        });
        it.skip("loads up convo manifest file manifest correctly", () => {
            expect(game.textures.exists("animals2")).to.be.true;
        });
        it.skip("loads up book manifest file manifest correctly", () => {
            expect(game.textures.exists("animals2")).to.be.true;
        });
        after(() => {
            game.destroy(true);
        });
    });
});


