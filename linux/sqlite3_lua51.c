// (c) 2018 Tom Schouten -- see LICENSE file
// Derived from erl_tools/c_src/sqlite3.c

#include <stdint.h>
#include <stdio.h>
#include <setjmp.h>
#include <sqlite3.h>
#include "lua.h"
#include "lauxlib.h"
#include "macros.h"
#include "assert_write.h"
/* For default.elf.do
ELF_LDLIBS=-lsqlite3
*/


/* Based on bert_writer from erl_tools/include/bert.h
   Same idea here: write things to Lua
*/



static const char *string_L(lua_State *L, int index, size_t *len) {
    ASSERT(lua_isstring(L, index));
    if (len) {
        return lua_tolstring(L, index, len);
    }
    else {
        return lua_tostring(L, index);
    }
}


/* Wrappers for DB type. */
struct db {
    sqlite3 *db;
    sqlite3_stmt *stmt;
};
void db_init(struct db *db) {
}
#define db_T "uc_tools.sqlite3"
#define NS(name) CONCAT(db,name)
#include "ns_lua_struct.h"
#undef NS

// https://www.sqlite.org/c_interface.html
// https://www.sqlite.org/quickstart.html

int db_query_cmd(lua_State *L) {
    struct db *db = &db_L(L, 1)->base;
    ASSERT(lua_istable(L, 2));

    /* The table is an array with first element the query string, and
       other elements containing query arguments. */
    lua_rawgeti(L, 2, 1);
    size_t query_len;
    const char *query = string_L(L, -1, &query_len);

    /* First term is the query string. */
    int rv = sqlite3_prepare_v2(db->db, query, query_len, &db->stmt, NULL);
    if (rv != SQLITE_OK) {
        goto error;
    }

    /* Subsequent terms are query arguments.  Note that it does matter
       whether we bind blobs or strings.  For now, just use strings as
       default and later add blob support, e.g. {"blob", <luastring>} */
    int arg_nb = 0;
    for(;;) {
        size_t arg_len;
        lua_rawgeti(L, 2, arg_nb + 2);
        if (lua_isnil(L, -1)) break;
        /* Only Lua string arguments are supported for now. */
        const char *arg = string_L(L, -1, &arg_len);
        // sqlite_bind_blob: TODO, maybe other types?
        if (SQLITE_OK != sqlite3_bind_text(
                db->stmt, arg_nb + 1, arg, arg_len,
                /* Strings will be stable during this Lua function
                   call.  Not sure if SQLITE will hang on to them, so
                   don't use SQLITE_STATIC here. */
                SQLITE_TRANSIENT)) {
            goto error;
        }
        arg_nb++;
    }

    /* All results are included in table of rows. */
    lua_newtable(L);


    /* Step through the query results, sending out rows as list of
       binaries. */
    int row = 0;
    while(1) {
        int rv = sqlite3_step(db->stmt);
        if (rv == SQLITE_ROW) {

            /* Lua table to contain this row. */
            lua_pushnumber(L, row+1);
            lua_newtable(L);

            /* Get rows converted to binary */
            int nb_cols = sqlite3_column_count(db->stmt);

            for (int col=0; col<nb_cols; col++) {
                /* Return everything as blobs / Lua strings */
                const char *data = sqlite3_column_blob(db->stmt, col);
                size_t len       = sqlite3_column_bytes(db->stmt, col);
                lua_pushnumber(L, col+1);
                lua_pushlstring(L, data, len);
                lua_settable(L, -3);
            }

            /* Store the row table into the table of rows. */
            lua_settable(L, -3);
        }
        else if (rv == SQLITE_DONE) {
            break;
        }
        else {
            goto error;
        }
        row++;
    }
    sqlite3_finalize(db->stmt);
    db->stmt=NULL;
    return 1;

  error:
    {
        const char *msg = sqlite3_errmsg(db->db);
        LOG("sql_error %d %s\n", rv, msg);
        lua_pushboolean(L, 0);
        lua_pushstring(L, msg);
        return 2;
    }

}


int db_open_cmd(lua_State *L) {
    struct db *db = &db_L(L, -2)->base;
    const char *db_file = string_L(L, -1, NULL);
    if (db->db) {
        LOG("closing previous db\n");
        sqlite3_close(db->db);
    }
    if ((SQLITE_OK != sqlite3_open(db_file, &db->db))) {
        LOG("%s: %s\n", db_file, sqlite3_errmsg(db->db));
        sqlite3_close(db->db);
        exit(1);
    }
    sqlite3_enable_load_extension(db->db, 1);
    return 0;
}


//ssize_t raw_read(int fd, void *buf, size_t count);
//ssize_t raw_write(int fd, const void *buf, size_t count);

// FIXME: wrap setjump around query?
        /* if(!setjmp(error_jmp_buf)) { */
        /*     // TRY */
        /*     query(msg,msg_len); */
        /* } */
        /* else { */
        /*     // CATCH */
        /*     sqlite3_finalize(stmt); stmt=NULL; */
        /* } */
        /* free(msg); */




#define CMD(_name) { \
    lua_pushcfunction (L, _name##_cmd); \
    lua_setfield (L, -2, #_name); \
    }

static void new_metatable(lua_State *L, const char *t_name, int (*gc)(lua_State *)) {
    luaL_newmetatable(L, t_name);
    if (1) {
        luaL_getmetatable(L, t_name);
        lua_pushstring(L, t_name); lua_setfield(L, -2, "__name");
        lua_pushcfunction(L, gc);  lua_setfield(L, -2, "__gc");
        lua_pop(L, -1);
    }
}


int luaopen_sqlite3_lua51 (lua_State *L) {

    new_metatable(L, db_T, db_gc);

    lua_newtable(L);
    CMD(db_new);
    CMD(db_open);
    CMD(db_query);


    return 1;
}
