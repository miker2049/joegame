//hmm
import { Toner } from '../src/index'
import { chaipromises, expect, parseCSVRowsToWikiData, joegameFacade, testdata } from './imports'

describe('the Toner object', function() {
    describe('hardcoded synths imported to a map', function() {
        let tonee
        before(function() {
            tonee = new Toner()
        })
        it('will not crash if it doesnt have the supplied synth ID', function() {
            expect(tonee.play('ksadjnlaksd')).to.be.undefined
        })
        it('will not crash if it doesnt have the supplied synth ID', function() {
            expect(tonee.play('arp')).to.be.undefined
            expect(tonee.play('gong')).to.be.undefined
            expect(tonee.play('walk')).to.be.undefined
        })
    })
})
