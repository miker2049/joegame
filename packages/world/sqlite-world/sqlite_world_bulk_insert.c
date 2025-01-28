#include <sqlite3.h>
#include <stddef.h>
#include <stdint.h>    // for uint32_t and uint8_t
typedef struct {
    sqlite3 *db;
    sqlite3_stmt *stmt;
} BulkInsert;

int bulk_insert_init(BulkInsert *bi, const char *dbpath) {
    char *sErrMsg = NULL;
    const char *sql = "INSERT INTO data (idx, value) VALUES (?, ?)";
    sqlite3_config(SQLITE_CONFIG_SINGLETHREAD);
    if (sqlite3_open_v2(dbpath, &bi->db, (SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE | SQLITE_OPEN_NOMUTEX) ,NULL) != SQLITE_OK) return -1;
    sqlite3_exec(bi->db, "PRAGMA synchronous = OFF", NULL, NULL, &sErrMsg);
    sqlite3_exec(bi->db, "PRAGMA journal_mode = WAL", NULL, NULL, &sErrMsg);
    if (sErrMsg) {
        sqlite3_free(sErrMsg);
        return -1;
    }
    if (sqlite3_prepare_v2(bi->db, sql, -1, &bi->stmt, NULL) != SQLITE_OK) return -1;
    sqlite3_exec(bi->db, "BEGIN TRANSACTION", NULL, NULL, NULL);
    return 0;
}

int bulk_insert_row(BulkInsert *bi, uint32_t idx, uint8_t value) {
    // Bind values to prepared statement
    // Call sqlite3_bind_* functions here based on your schema

    sqlite3_bind_int(bi->stmt, 1, idx);
    sqlite3_bind_int(bi->stmt, 2, value);
    if (sqlite3_step(bi->stmt) != SQLITE_DONE) return -1;
    sqlite3_reset(bi->stmt);
    return 0;
}

void bulk_insert_finish(BulkInsert *bi) {
    sqlite3_exec(bi->db, "END TRANSACTION", NULL, NULL, NULL);
    sqlite3_finalize(bi->stmt);
    sqlite3_close(bi->db);
}
