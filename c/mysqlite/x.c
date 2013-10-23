#include <stdio.h>
#include <sqlite3.h>
#include <stdlib.h>
#include <string.h>
#include "mysqlite.h"

int main ()
{
    sqlite3 * db;
    char * sql;
    sqlite3_stmt * stmt;
    int i;

    CALL_SQLITE (open ("test.db", & db));
    sql = "INSERT INTO t (xyz) VALUES (?)";
    CALL_SQLITE (prepare_v2 (db, sql, strlen (sql) + 1, & stmt, NULL));
    CALL_SQLITE (bind_text (stmt, 1, "fruit", 6, SQLITE_STATIC));
    CALL_SQLITE_EXPECT (step (stmt), DONE);
    printf ("row id was %d\n", (int) sqlite3_last_insert_rowid (db));
    return 0;
}
