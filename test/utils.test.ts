import * as Phaser from 'phaser'
import { expect } from 'chai'
import { getVolAndPanFromDistance } from '../src/utils/getVolPanFromDist'
import loadAfterLoad from '../src/utils/loadAfterLoad'
import { hashToArr } from '../src/utils/hashToArr'
import { syllableCount } from '../src/utils/syllableCount'
import { getTestScene } from './testutils/test-scene-config'

describe('hashToArr function', function() {
    it('returns correct number of positive single digit integers', function() {
        ['test', 'howdy.', 'whasakjndsa221', 'aksjdq01j*wkj/', '2198123&&^2712131<>1928'].forEach(str => {

            const result = hashToArr(str, 3)
            expect(result).to.have.lengthOf(3)
            expect(result[0]).to.be.a('number').and.to.not.be.below(0)
            expect(result[1]).to.be.a('number').and.to.not.be.below(0)
            expect(result[2]).to.be.a('number').and.to.not.be.below(0)

            expect(result[0]).to.not.be.above(9)
            expect(result[1]).to.not.be.above(9)
            expect(result[2]).to.not.be.above(9)
        })
        // expect(result).to.not.include.a
    })
    it('will always be able to return twice the amount of numbers as syllables found', function() {
        ['capitol', 'personality', 'parlimentarian', 'humanitarian', 'richardsonsteinstipple'].forEach(item => {
            const syll = syllableCount(item)
            const hash = hashToArr(item, syll * 2)
            expect(hash).to.have.lengthOf(syll * 2)
        })
    })
})

describe('get vol and pan from distance', function() {
    it('will return a tuple with reasonable vol and pan modifiers based on a distance', function() {
        const first = getVolAndPanFromDistance(150, 150, 0, 0, 800)
        console.log(getVolAndPanFromDistance(15000, 150, 0, 0, 800))
        console.log(getVolAndPanFromDistance(150, 150, 150, 150, 800))
        expect(first[0]).to.be.greaterThan(0)
    })
})

describe('loadAfterLoad function', function() {
    let scene, loaded
    before(async function() {

        scene = await getTestScene()
        // const scene = game.scene.add('test', {})
        loaded = await loadAfterLoad(scene, 'test-loadAfterload', 'assets/images/canyon3.png', 'image')
    })
    it('loads file as a promise that returns the key from the loader', async function() {
        expect(loaded).to.be.an('string')
        scene.game.destroy(true)
    })
})
