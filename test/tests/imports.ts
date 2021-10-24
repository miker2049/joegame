import chaiPromise from 'chai-as-promised'
import chai from 'chai'
import joegameFacade from '../../src/joegameFacade'
import { parseCSVRowsToGameData } from '../../src/utils/parseCSVRowsToGameData'
import loadConvoManifestJSON from '../../src/utils/loadConvoManifestJSON'
import testdataa from '../../assets/data.csv'
import { getMapKeyNameRaw } from '../../src/utils/getKeyNames'
const expect = chai.expect
chai.use(chaiPromise)

const TESTMAPPATH = 'assets/maps/testmap.json'
export {
    expect,
    joegameFacade,
    parseCSVRowsToGameData,
    testdataa,
    getMapKeyNameRaw,
    loadConvoManifestJSON,
    TESTMAPPATH

}
