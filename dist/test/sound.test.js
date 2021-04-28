import { Toner } from '../dist/index';
import { expect } from './imports';
describe('the Toner object', function () {
    describe('hardcoded synths imported to a map', function () {
        let tonee;
        before(function () {
            tonee = new Toner(new AudioContext());
        });
        it('will not crash if it doesnt have the supplied synth ID', function () {
            expect(tonee.play('ksadrjnlaksd')).to.be.undefined;
        });
        it('will not crash if it doesnt have the supplied synth ID', function () {
            expect(tonee.play('arp')).to.be.undefined;
            expect(tonee.play('gong')).to.be.undefined;
            expect(tonee.play('walk')).to.be.undefined;
        });
    });
});
//# sourceMappingURL=sound.test.js.map