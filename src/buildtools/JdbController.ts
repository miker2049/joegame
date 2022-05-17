import sqlite3, { Database } from 'sqlite3'
import { JdbAsset, JdbBody, JdbModels, JdbTableNames, JdbTable } from '../buildtools/JdbModel'

async function asyncGet<T>(db: Database, stmt: string, params: any): Promise<T> {
    return new Promise((res, rej) => {

        db.get(stmt, params, (err, rows) => {
            if (err) {
                rej(Error("A sqlite error"))
            } else if(!rows){
                rej(Error("No rows.."))
            } else {
                res(rows as T)
            }
        })
    })
}

/*
 * If you delete a row, there will be holes in the ids, so there is no guarantee here
 * the length of the returned array.
 */
async function asyncGetNCols<T, C>(db: Database, table: string, col: string, n: number, offset: number = 0): Promise<C[]>{
    let found: C[] = []
    let $cur = offset
    let canidate: T | undefined

    while($cur < n){
        console.log($cur)
        try {
            canidate = await asyncGet<T>(db,`SELECT ${col} FROM ${table} WHERE ${col} = $cur;`, { $cur })
            if(canidate[col]){
                found.push(canidate[col])
            }
        } catch (err) {
            console.log(err)
        }
        $cur += 1
    }
    return found
}

async function asyncRun(db: Database, stmt: string, params: any) {
    return new Promise((res, rej) => {
        db.run(stmt, params, (err, _rows) => {
            if (err) {
                throw err
            }
            res(undefined)
        })
    })
}


function formatMoneySign(input: any[]): string[]{
   return input.map(item=>'$'+item)
}
function formatColumnList(input: any[]): string {
   return input.join(', ')
}

function formatParens(input: string): string {
    return `( ${input} )`
}

function formatWhere<T>(cols: (keyof T)[] | string[]){
   return "WHERE "+Object.keys(cols).map(item=>`${item}=${'$'+item}`).join(" AND ")
}

function formatInsertInto<T>(table: string, cols: (keyof T)[] | string[]): string {
    return `INSERT INTO ${table}${formatParens(formatColumnList(cols))} VALUES ${formatParens(formatColumnList(formatMoneySign(cols)))}`
}
function formatSelect<T>(table: string, cols: (keyof T)[] | string[]): string {
    return `SELECT ${formatParens(formatColumnList(cols))} FROM ${table}}`
}

function formatUpdate<T>(table: string, cols: (keyof T)[] | string[]): string{
     return `UPDATE OR FAIL ${table} SET${formatParens(formatColumnList(cols))} = ${formatParens(formatColumnList(formatMoneySign(cols)))}`
}

function absorbProps<T>(target: T, source: Partial<T>){
    Object.keys(target).forEach(key=>{
        if(source[key]){
           target[key] = source[key]
        }
    })
    return target
}

function convertBase64<T>(obj: T): T{
    let out = {}
    Object.keys(obj).forEach(k=>{
        if(k.match("base64_")){
            out[k.replace("base64_","")] = Buffer.from(obj[k], "base64")
        } else {
            out[k] = obj[k]
        }
    });
    return out as T
}
function formatParamKeys(obj: object){
    let out = {}
    Object.keys(obj).forEach(k=>out['$'+k]=obj[k]);
    return out
}

export default class JdbController {
    db: Database
    model: JdbModels
    constructor(dbPath: string) {
        this.db = new sqlite3.Database(dbPath)
        this.model = new JdbModels()
    }

    async insertRow<T extends JdbTable>(table: JdbTableNames, input: Partial<T>): Promise<number> {
        const t_table = this.model.schema.get(table)
        if (!t_table) throw Error("Table not found")
        input = convertBase64(input)
        const data: JdbTable = Object.assign(t_table, input)
        delete data.id
        await asyncRun(this.db,
                       `${formatInsertInto<T>(table,Object.keys(data))}`,
                       formatParamKeys(data))
        const out = await asyncGet(this.db, "SELECT last_insert_rowid();", [])
        return out as number
    async getIds<T extends JdbTable>(table: JdbTableNames, limit: number, offset: number): Promise<number[]>{
        const out = await asyncGetNCols<T,number>(this.db,table,"id", limit, offset)
        return out
    }

    async selectById<T extends JdbTable>(table: JdbTableNames, id: number): Promise<T> {
        return await asyncGet<T>(this.db, `SELECT * FROM ${table} WHERE id=$id`, {$id: id})
    }

    async updateRow<T extends JdbTable>(table: JdbTableNames, input: Partial<T> & {id: number}): Promise<{id: number}> {
        if(!input.id) throw Error("No ID given to update")
        input = convertBase64(input)
        let data: T = absorbProps<T>(await this.selectById<T>(table,input.id), input)
        const id = data.id
        delete data.id
        const cols: string[] = Object.keys(data)
        await asyncRun(this.db,
                       `${formatUpdate<T>(table,cols)} WHERE id=$id`,
                       formatParamKeys({id: id,...data}))
        return {id: input.id}
    }

    async deleteRow(table: JdbTableNames, id: number): Promise<boolean> {
        await asyncRun(this.db,`DELETE FROM ${table} WHERE id=$id`, {$id: id})
        return true
    }
}
