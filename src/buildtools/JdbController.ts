import sqlite3, { Database } from 'sqlite3'
import { JdbAsset, JdbBody, JdbModels, JdbTableNames, JdbTable } from '../buildtools/JdbModel'

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
   return "WHERE "+Object.keys(cols).map(item=>`${item}=${'$'+item}`).join(" and ")
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
        const data: JdbTable = Object.assign(t_table, input)
        delete data.id

        await asyncRun(this.db,`${formatInsertInto<T>(table,data.getCols())}`, data)
        const out = await asyncGet(this.db, "SELECT last_insert_rowid();", [])
        return out as number
    }

    async selectById<T extends JdbTable>(table: JdbTableNames, id: number): Promise<T> {
        return await asyncGet<T>(this.db, `SELECT * FROM ${table} WHERE id=$id`, {$id: id})
    }

    async updateRow<T extends JdbTable>(table: JdbTableNames, input: Partial<T> & {id: number}): Promise<{id: number}> {
        if(!input.id) throw Error("No ID given to update")
        const data: T = absorbProps<T>(await this.selectById<T>(table,input.id), input)
        const cols: string[] = data.getCols()
        await asyncRun(this.db,`${formatUpdate<T>(table,cols)} WHERE id=$id`, data)
        return {id: input.id}
    }

    async deleteRow(table: JdbTableNames, id: number): Promise<boolean> {
        await asyncRun(this.db,`DELETE FROM ${table} WHERE id=$id`, {$id: id})
        return true
    }
}
