import sqlite3, { Database } from 'sqlite3'
import { Asset, Body } from '../types/jdb-types'

async function asyncGet<T>(db: Database, stmt: string, params: any): Promise<T> {
    return new Promise((res, rej) => {
        db.get(stmt, params, (err, rows) => {
            if (err) {
                throw err
            }
            res(rows as T)
        })
    })
}
async function asyncRun(db: Database, stmt: string, params: any) {
    return new Promise((res, rej) => {
        console.log(stmt)
        console.log(params)
        db.run(stmt, params, (err, _rows) => {
            if (err) {
                throw err
            }
            res(undefined)
        })
    })
}

async function updateRow<T>(input: Partial<T>, id: number, db: Database, sql: string, current: T): Promise<void> {
    let out = Object.assign(current, input)
    let params = {}
    Object.keys(out).forEach(key=> params['$'+key]=out[key])
    await asyncRun(db, sql, params)
    // const last = await asyncGet(db, 'SELECT last_insert_rowid();', [])
    // return last as number
}

export default class JdbController {
    db: Database
    constructor(dbPath: string) {
        this.db = new sqlite3.Database(dbPath)
    }


    async createAsset({ name, hash, creator, asset_source, asset_type, blob_data }: Asset): Promise<number> {
        await asyncRun(this.db, 'INSERT INTO assets(filename, hash, creator,asset_type) VALUES (?,?,?,?)',
            [name, hash, creator, asset_type])
        const out = await asyncGet(this.db, "SELECT last_insert_rowid();", [])
        return out as number
    }

    async getAssetById(id: number): Promise<Asset> {
        const out: Asset = await asyncGet<Asset>(this.db, "SELECT * FROM assets WHERE id=?", [id])
        return out
    }

    async updateAsset(asset: Partial<Asset>, id: number): Promise<void> {
        const curr = await this.getAssetById(id)
        await updateRow<Asset>(asset,id,this.db, 'UPDATE OR FAIL assets SET(filename,hash,creator,asset_source, asset_type, blob_data)=($filename,$hash,$creator,$asset_source, $asset_type, $blob_data) WHERE id=$id;', curr)

    }
    //delete asset

    async createBody(input: Body) {
        await asyncRun(this.db, `INSERT INTO bodies(anim_north, anim_south,
anim_east,anim_west, speed, dash_distance,scale,body_offset_x,body_offset_y,
width,height) VALUES ($anim_north, $anim_south,
$anim_east,$anim_west, $speed, $dash_distance,$scale,$body_offset_x,$body_offset_y,
$width,$height)`,
            {
                $anim_north: input.anim_north ?? 1,
                $anim_south: input.anim_south ?? 1,
                $anim_east: input.anim_east ?? 1,
                $anim_west: input.anim_west ?? 1,
                $speed: input.speed ?? 1,
                $dash_distance: input.dash_distance ?? 1,
                $scale: input.scale ?? 1,
                $body_offset_x: input.body_offset_x ?? 0,
                $body_offset_y: input.body_offset_y ?? 0,
                $width: input.width ?? 0,
                $height: input.height ?? 0
            })
        const out = await asyncGet(this.db, "SELECT last_insert_rowid();", [])
        return out as number
    }

}
