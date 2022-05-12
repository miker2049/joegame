"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const tslib_1 = require("tslib");
const sqlite3_1 = (0, tslib_1.__importDefault)(require("sqlite3"));
function asyncGet(db, stmt, params) {
    return (0, tslib_1.__awaiter)(this, void 0, void 0, function* () {
        return new Promise((res, rej) => {
            db.get(stmt, params, (err, rows) => {
                if (err) {
                    throw err;
                }
                res(rows);
            });
        });
    });
}
function asyncRun(db, stmt, params) {
    return (0, tslib_1.__awaiter)(this, void 0, void 0, function* () {
        return new Promise((res, rej) => {
            console.log(stmt);
            console.log(params);
            db.run(stmt, params, (err, _rows) => {
                if (err) {
                    throw err;
                }
                res(undefined);
            });
        });
    });
}
function updateRow(input, id, db, sql, current) {
    return (0, tslib_1.__awaiter)(this, void 0, void 0, function* () {
        let out = Object.assign(current, input);
        let params = {};
        Object.keys(out).forEach(key => params['$' + key] = out[key]);
        yield asyncRun(db, sql, params);
        // const last = await asyncGet(db, 'SELECT last_insert_rowid();', [])
        // return last as number
    });
}
class JdbController {
    constructor(dbPath) {
        this.db = new sqlite3_1.default.Database(dbPath);
    }
    createAsset({ name, hash, creator, asset_source, asset_type, blob_data }) {
        return (0, tslib_1.__awaiter)(this, void 0, void 0, function* () {
            yield asyncRun(this.db, 'INSERT INTO assets(filename, hash, creator,asset_type) VALUES (?,?,?,?)', [name, hash, creator, asset_type]);
            const out = yield asyncGet(this.db, "SELECT last_insert_rowid();", []);
            return out;
        });
    }
    getAssetById(id) {
        return (0, tslib_1.__awaiter)(this, void 0, void 0, function* () {
            const out = yield asyncGet(this.db, "SELECT * FROM assets WHERE id=?", [id]);
            return out;
        });
    }
    updateAsset(asset, id) {
        return (0, tslib_1.__awaiter)(this, void 0, void 0, function* () {
            const curr = yield this.getAssetById(id);
            yield updateRow(asset, id, this.db, 'UPDATE OR FAIL assets SET(filename,hash,creator,asset_source, asset_type, blob_data)=($filename,$hash,$creator,$asset_source, $asset_type, $blob_data) WHERE id=$id;', curr);
        });
    }
    //delete asset
    createBody(input) {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k, _l;
        return (0, tslib_1.__awaiter)(this, void 0, void 0, function* () {
            yield asyncRun(this.db, `INSERT INTO bodies(anim_north, anim_south,
anim_east,anim_west, speed, dash_distance,scale,body_offset_x,body_offset_y,
width,height) VALUES ($anim_north, $anim_south,
$anim_east,$anim_west, $speed, $dash_distance,$scale,$body_offset_x,$body_offset_y,
$width,$height)`, {
                $anim_north: (_a = input.anim_north) !== null && _a !== void 0 ? _a : 1,
                $anim_south: (_b = input.anim_south) !== null && _b !== void 0 ? _b : 1,
                $anim_east: (_c = input.anim_east) !== null && _c !== void 0 ? _c : 1,
                $anim_west: (_d = input.anim_west) !== null && _d !== void 0 ? _d : 1,
                $speed: (_e = input.speed) !== null && _e !== void 0 ? _e : 1,
                $dash_distance: (_f = input.dash_distance) !== null && _f !== void 0 ? _f : 1,
                $scale: (_g = input.scale) !== null && _g !== void 0 ? _g : 1,
                $body_offset_x: (_h = input.body_offset_x) !== null && _h !== void 0 ? _h : 0,
                $body_offset_y: (_j = input.body_offset_y) !== null && _j !== void 0 ? _j : 0,
                $width: (_k = input.width) !== null && _k !== void 0 ? _k : 0,
                $height: (_l = input.height) !== null && _l !== void 0 ? _l : 0
            });
            const out = yield asyncGet(this.db, "SELECT last_insert_rowid();", []);
            return out;
        });
    }
}
exports.default = JdbController;
