import chaiPromise from 'chai-as-promised'
import chai from 'chai'
import joegameFacade from '../src/joegameFacade'
import { parseCSVRowsToWikiData } from '../src/utils/parseCSVRowsToWikiData'
import loadConvoManifestJSON from '../src/utils/loadConvoManifestJSON'
import testdataa from '../assets/data.csv'
import { getMapKeyNameRaw } from '../src/utils/getKeyNames'
const expect = chai.expect
console.log('')
chai.use(chaiPromise)
const TESTMAPPATH = 'assets/maps/testmap.json'
export {
    expect,
    joegameFacade,
    parseCSVRowsToWikiData,
    testdataa,
    getMapKeyNameRaw,
    loadConvoManifestJSON,
    TESTMAPPATH

}
