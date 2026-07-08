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

static inline void dbg_free(const char *tag, void *ptr) {
    LOG("%s %p\n", tag, ptr);
    // FIXME: Something isn't right in how these pointers are tracked.
    // For now it works just letting it leak.
    // free(ptr);
    //LOG("dbg_free %s %p ok\n", tag, ptr);
}


/* Wrappers for DB type. */
struct db_handle {
    sqlite3 *db;
    uintptr_t rc; // 0 means it's the last one
};
struct db {
    struct db_handle *handle;
    sqlite3_stmt *stmt;
    int row;
};
void db_init(struct db *db) {
    memset(db, 0, sizeof(*db));
}
void db_free(struct db *db) {
    if (db->handle) {
        if (db->handle->rc > 0) {
            db->handle->rc--;
        }
        else {
            dbg_free("db_free", db->handle->db);
            db->handle->db = NULL;
        }
    }
    db->handle = NULL;
}
#define db_T "uc_tools.sqlite3"
#define NS(name) CONCAT(db,name)
#define NS_LUA_STRUCT_FREE db_free
#include "ns_lua_struct.h"
#undef NS_LUA_STRUCT_FREE
#undef NS

/* Note that originally we only supported query-to-table translations.
   The need arose to do incremental queries.  The data needed for
   those is the same as the "main" database handle object.  We
   distinguish the two by the .is_cursor flag. */


// https://www.sqlite.org/c_interface.html
// https://www.sqlite.org/quickstart.html

static inline int db_error(struct lua_State *L, const char *msg) {
    LOG("db_error: %s\n", msg);
    lua_pushboolean(L, 0);
    lua_pushstring(L, msg);
    return 2;
}

int db_dup_cmd(lua_State *L) {
    struct db *db_old = &db_L(L, 1)->base;
    db_new_cmd(L);
    struct db *db_new = &db_L(L, 2)->base;
    ASSERT(db_new->handle == NULL);
    if (db_old->handle) {
        db_new->handle = db_old->handle;
        db_new->handle->rc++;
    }
    return 1; // Just return new
}

int db_return_error(lua_State *L, struct db *db, int rv) {
    const char *msg = sqlite3_errmsg(db->handle->db);
    LOG("sql_error %d %s\n", rv, msg);
    lua_pushboolean(L, 0);
    lua_pushstring(L, msg);
    return 2;
}


int db_prepare_and_bind(lua_State *L, struct db *db) {
    /* The table is an array with first element the query string, and
       other elements containing query arguments. */
    lua_rawgeti(L, 2, 1);
    size_t query_len;
    const char *query = string_L(L, -1, &query_len);

    /* First term is the query string. */
    int rv = sqlite3_prepare_v2(db->handle->db, query, query_len, &db->stmt, NULL);
    if (rv != SQLITE_OK) return db_return_error(L, db, rv);

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
        if (SQLITE_OK != (rv = sqlite3_bind_text(
                db->stmt, arg_nb + 1, arg, arg_len,
                /* Strings will be stable during this Lua function
                   call.  Not sure if SQLITE will hang on to them, so
                   don't use SQLITE_STATIC here. */
                SQLITE_TRANSIENT))) {
            return db_return_error(L, db, rv);
        }
        arg_nb++;
    }
    /* All good, we can continue.
       If not good = early exit, rv >= 0 and stack contains return values. */
    return -1;
}

void db_push_row(lua_State *L, struct db *db, int row) {
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
}

void db_need_finalized(struct db *db) {
    if (db->stmt) {
        sqlite3_finalize(db->stmt);
        db->stmt = NULL;
    }
}

int db_query_cmd(lua_State *L) {
    struct db *db = &db_L(L, 1)->base;
    ASSERT(lua_istable(L, 2));

    int rv;
    if ((rv = db_prepare_and_bind(L, db)) >= 0) return rv;


    /* All results are included in table of rows. */
    lua_newtable(L);

    /* Step through the query results, sending out rows as list of
       binaries. */
    int row = 0;
    while(1) {
        int rv = sqlite3_step(db->stmt);
        if (rv == SQLITE_ROW) {
            /* Collect results from current row into Lua table. */
            db_push_row(L, db, row);
            /* Store the row table into the table of rows. */
            lua_settable(L, -3);
        }
        else if (rv == SQLITE_DONE) {
            break;
        }
        else {
            return db_return_error(L, db, rv);
        }
        row++;
    }
    sqlite3_finalize(db->stmt);
    return 1;
}


int db_next_cmd(lua_State *L) {
    struct db *db = &db_L(L, 1)->base;
    int rv = sqlite3_step(db->stmt);
    if (rv == SQLITE_ROW) {
        /* Collect results from current row into Lua table. */
        db_push_row(L, db, db->row++);
        return 1;
    }
    else if (rv == SQLITE_DONE) {
        db_need_finalized(db);
        return 0;
    }
    else {
        return db_return_error(L, db, rv);
    }
}


int db_start_cmd(lua_State *L) {
    struct db *db = &db_L(L, 1)->base;
    ASSERT(lua_istable(L, 2));
    int rv;
    if ((rv = db_prepare_and_bind(L, db)) >= 0) return rv;
    lua_pushboolean(L, 1);
    return 1;
}




int db_open_cmd(lua_State *L) {
    struct db *db = &db_L(L, 1)->base;
    const char *db_file = string_L(L, 2, NULL);

    /* If multiple opens are needed then create multiple objects
       instead. */
    ASSERT(db->handle == NULL);
    db->handle = calloc(1, sizeof(*db->handle));

    if ((SQLITE_OK != sqlite3_open(db_file, &db->handle->db))) {
        LOG("%s: %s\n", db_file, sqlite3_errmsg(db->handle->db));
        sqlite3_close(db->handle->db);
        dbg_free("db_open", db->handle);
        db->handle = NULL;
        exit(1);
    }
    sqlite3_enable_load_extension(db->handle->db, 1);

    int timeout_ms = 5000;
    sqlite3_busy_timeout(db->handle->db, timeout_ms);

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
    CMD(db_dup);

    CMD(db_start);
    CMD(db_next);


    return 1;
}
