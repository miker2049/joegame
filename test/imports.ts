import * as chaiPromise from 'chai-as-promised'
import { expect, use as chaiUse } from 'chai'
import joegameFacade from '../dist/joegameFacade'
import { parseCSVRowsToWikiData } from '../dist/utils/parseWikiData'
import loadConvoManifestJSON from '../dist/utils/loadConvoManifestJSON'
import testdata from './assets/data.csv'
import { getMapKeyNameRaw } from '../dist/utils/getKeyNames'

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
