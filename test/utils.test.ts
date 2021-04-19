import { expect } from 'chai'
import hashToArr from '../src/utils/hashToArr'

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
})
