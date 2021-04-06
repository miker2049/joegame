import * as chaiPromise from 'chai-as-promised'
import { expect, use as chaiUse } from 'chai'
import joegameFacade from '../src/joegameFacade'
import { parseCSVRowsToWikiData } from '../src/utils/parseWikiData'
import loadConvoManifestJSON from '../src/utils/loadConvoManifestJSON'
import testdata from './assets/data.csv'
import { getMapKeyNameRaw } from '../src/utils/getKeyNames'

const chaipromises = () => chaiUse(chaiPromise)
export {
    chaipromises,
    expect,
    joegameFacade,
    parseCSVRowsToWikiData,
    testdata,
    getMapKeyNameRaw,
    loadConvoManifestJSON
}
