// import {getMapKeyName, getMapKeyNameRaw, getKeyName, getSceneKeyName} from '../src/levelLoader'
import 'phaser'
import * as sinon from 'sinon'
import * as chai from 'chai'
import * as chaiThings from 'chai-things'
import getTileFromPoint from '../src/utils/getTileFromPoint'

import joegameFacade from '../src/joegameFacade'
chai.use(chaiThings)
let expect = chai.expect

describe('the getTileFromPoint function', function(){
    it('works with negative numbers', function(){
        let result = getTileFromPoint({x: -12, y: -23}, 16)
        expect(result.x).to.equal(-1)
        expect(result.y).to.equal(-2)
        let result2 = getTileFromPoint({x: -40, y: -26.3}, 16)
        expect(result2.x).to.equal(-3)
        expect(result2.y).to.equal(-2)
    })
    it('works with zeros', function(){
        let result = getTileFromPoint({x: 7, y: 5}, 16)
        expect(result).to.have.property('x',0)
        expect(result).to.have.property('y',0)
        // let result2 = getTileFromPoint({x: 32.2, y: 16.3}, 16)
        // expect(result2.x).to.equal(3)
        // expect(result2.y).to.equal(2)
        let result3 = getTileFromPoint({x: 7, y: 11}, 16)
        expect(result3).to.have.property('x',0)
        expect(result3).to.have.property('y',0)
    })
})

describe('joegame init functions, joegameFacade', function(){
    let facade
    this.beforeAll('setup a joegameFacade', function(){
        facade = new joegameFacade()
    })
    describe('the game is initialiazed', function(){
        let game
        this.beforeAll('start the game', function(done){
           facade.initGame().then(val=>{
               game = val
               done()
           })
        })
        this.afterAll('close the game', function(){
            game.destroy(true)
        })
        it('successfully load phaser game', function(){
            expect(game).to.be.an.instanceOf(Phaser.Game)
        })

    })
})
