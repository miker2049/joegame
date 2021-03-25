//ts
import 'phaser'
import * as sinon from 'sinon'
import * as chai from 'chai'
import {createJoegameConfig} from '../src/createJoegameConfig'
import {getMapKeyName} from '../src/levelLoader'
import SceneMap from '../src/SceneMap'
const wikidatajson = require('../src/wikidata.json');
const testmapjson = require('../src/assets/testmap.json');
import * as chaiThings from 'chai-things'

chai.use(chaiThings)
let expect = chai.expect
