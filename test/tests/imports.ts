import { joegameFacade } from "joegamelib/src/joegameFacade";
import { parseCSVRowsToGameData } from "joegamelib/src/utils/parseCSVRowsToGameData";
import loadConvoManifestJSON from "joegamelib/src/utils/loadConvoManifestJSON";
import { getMapKeyNameRaw } from "joegamelib/src/utils/getKeyNames";

import testdataa from "../../assets/data.csv?raw";
const expect = chai.expect;
// chai.use(chaiPromise);
const TESTMAPPATH = "assets/maps/testmap.json";
export {
    expect,
    joegameFacade,
    parseCSVRowsToGameData,
    testdataa,
    getMapKeyNameRaw,
    loadConvoManifestJSON,
    TESTMAPPATH,
 };
