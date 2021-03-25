import 'phaser'
import * as sinon from 'sinon'
import * as chai from 'chai'

const wikidatajson = require('../src/wikidata.json');
const testmapjson = require('../src/assets/testmap.json');

import {parsewikidata} from '../src/parseWikiData'
import * as chaiThings from 'chai-things'
chai.use(chaiThings)
let expect = chai.expect
