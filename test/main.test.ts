// import {getMapKeyName, getMapKeyNameRaw, getKeyName, getSceneKeyName} from '../src/levelLoader'
import { expect } from 'chai'
import joegameFacade from '../src/joegameFacade'
import { parseCSVRowsToWikiData } from '../src/utils/parseWikiData'

describe('joegameFacade', () => {
    const fac = new joegameFacade()
    const truth = true
    // it('has properties', () => {
    //     expect(fac.initGame()).to.Throw
    // })
    it('can make functions', () => {
        expect('2').to.not.match(/3/)
        expect(truth).to.be.false
    })

    it('can make more functions', () => {
        expect('2').to.match(/2/)
    })
    after(() => {
        //cleanup
    })
})

