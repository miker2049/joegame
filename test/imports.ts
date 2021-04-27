import chaiPromise from 'chai-as-promised'
import chai from 'chai'
import joegameFacade from '../dist/joegameFacade'
import { parseCSVRowsToWikiData } from '../dist/utils/parseCSVRowsToWikiData'
import loadConvoManifestJSON from '../dist/utils/loadConvoManifestJSON'
import testdata from './assets/data.csv'
import { getMapKeyNameRaw } from '../dist/utils/getKeyNames'
const expect = chai.expect
chai.use(chaiPromise)
export {
    expect,
    joegameFacade,
    parseCSVRowsToWikiData,
    testdata,
    getMapKeyNameRaw,
    loadConvoManifestJSON
}
