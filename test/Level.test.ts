import 'phaser'
import * as sinon from 'sinon'
import * as chai from 'chai'
import {createJoegameConfig} from '../src/createJoegameConfig'
import { getMapKeyName, getSceneKeyName, loadLevel} from '../src/levelLoader'
// import SceneMap from '../src/SceneMap'
// const wikidatajson = require('../src/wikidata.json');
// const testmapjson = require('../src/assets/testmap.json');
import Level from '../src/Level'
let expect = chai.expect

describe('Initialize a phaser game for the Level class', function(){
    let jgame3: Phaser.Game
    let levelscene: Level
    const leveldata = {mapjson:'../assets/testmap.json',x: 13, y: 11, startLevel:false}

    before("load the assets of the testmap but do not start scene",function(done){
        jgame3= new Phaser.Game(createJoegameConfig(leveldata))
        this.timeout(5000)
        jgame3.events.once('assetsloaded', ()=>{
            done()
        })
    })
    after(function(){
        jgame3.destroy(true,false)
    })
    describe('the level class is run', function(){
        before('the level is added and initiated',function(done){
            levelscene=jgame3.scene.add(getSceneKeyName(leveldata.mapjson),
                            new Level({key:getSceneKeyName(leveldata.mapjson)}),
                            true,
                            { callbacks:[done],
                              ...leveldata}) as Level
            this.timeout(5000)
            // jgame.events.once('levelloaded', done())
        })
        it.skip('runs without error', function(){
            expect(jgame3.scene.getScene(getSceneKeyName(leveldata.mapjson))).to.be.an.instanceof(Level)
            expect(levelscene).to.be.an.instanceof(Level)
        })
        it.skip('creates a Level and is the only scene', function(){
            expect(jgame3.scene.getScene(getSceneKeyName(leveldata.mapjson))).not.to.be.null
            expect(jgame3.scene.getScenes().length).to.equal(1)
        })
        describe.skip('the scenemap', function(){
            it('has the expected number of layers', function(){
                expect(levelscene.map.tilemap.layers.length).to.equal(3)
            })
            it('has the expected layers', function(){
                expect(levelscene.map.tilemap.layers.map(l=>l.name)).to.contain("Main")
                expect(levelscene.map.tilemap.layers.map(l=>l.name)).to.contain("Ground")
                expect(levelscene.map.tilemap.layers.map(l=>l.name)).to.contain("highlight")
            })
        })
    })

})
