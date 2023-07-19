// This is erl_tools/c_src/sqlite3.c
//
// Will be modifying this into something that can be included in Lua
// while sticking to the erl_tools API as much as possible.  Inital
// version recorded verbatim to track changes.

// What does this need to be?  A lua wrapper seems to be most
// appropriate.

// (c) 2018 Tom Schouten -- see LICENSE file

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
    jmp_buf error_jmp_buf;
};
void db_init(struct db *db) {
}
#define db_T "uc_tools.sqlite3"
#define NS(name) CONCAT(db,name)
#include "ns_lua_struct.h"
#undef NS



// https://www.sqlite.org/c_interface.html
// https://www.sqlite.org/quickstart.html


/* Use the generalized right fold over the erlang term format provided
   in bert.[ch] to iterate over the data primitives in the order they
   appear in the stream.

   Ignore tuple and list structure, and primitive atoms except for
   atom and binary, effectively flattening and filtering the input
   into a sequence of binaries and atoms:

   E.g. {<<"insert into config (var,val) values (?,?)">>,
         [{text,<<"myvar">>},
          {blob,<<"myval">>}]}

   Flattens to these constructor calls in sequence:

     binary(<query string>)
     atom('text'),
     binary(<<"myvar">>)
     atom('blob')
     binary(<<"myval">>)

   The meaning of an atom is then to set the sqlite3 binding type of
   the subsequent binary.  After that, the binding type is restored to
   text.  Binaries are collected in a arg list.

   The first binary is the query string.  The remaining binaries are
   arg bindings. */

struct slice {
    const uint8_t *buf;
    uint32_t len;
};


void sql_error(struct db *db, int rv) {
    const char *msg = sqlite3_errmsg(db->db);
    LOG("sql_error %d %s\n", rv, msg);
    longjmp(db->error_jmp_buf, 1);
}

struct sql_row {
    struct slice *col;
    uint32_t nb_cols;
};

int db_query_cmd(lua_State *L) {
    struct db *db = &db_L(L, 1)->base;
    ASSERT(lua_istable(L, 2));

    /* The table is an array with first element the query string, and
       other elements containing blobs. */
    struct slice query[10] = {};  // FIXME: max size hardcoded
    int query_len = 0;
    for(int i=0;i<ARRAY_SIZE(query);i++) {
        // LOG("get element %d\n", i);
        lua_rawgeti(L, 2, i+1);
        if (lua_isnil(L, -1)) break;
        size_t len;
        query[i].buf = (const uint8_t*)string_L(L, -1, &len);
        query[i].len = len;
        query_len++;
    }

    /* All results are included in table of rows. */
    lua_newtable(L);

    /* First term is the query string. */
    int rv = sqlite3_prepare_v2(
        db->db, (const char*)query[0].buf, query[0].len, &db->stmt, NULL);
    if (rv != SQLITE_OK) {
        sql_error(db, rv);
    }

    /* Subsequent terms are optional blob values to bind to the
       statement. */
    for(uint32_t i=1; i<query_len; i++) {
        const struct slice *a = &query[i];
        sqlite3_bind_blob(db->stmt, i, a->buf, a->len, SQLITE_STATIC /*ok?*/);
    }

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

            ASSERT(nb_cols > 0);
            // if (!nb_cols) break;  // FIXME: This is probably not correct.

            for (int col=0; col<nb_cols; col++) {
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
            sql_error(db, rv);
        }
        row++;
    }
    sqlite3_finalize(db->stmt);
    db->stmt=NULL;

    return 1;
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
