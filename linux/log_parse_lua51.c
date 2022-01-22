#include <unistd.h>
#include <stdint.h>

/* Lua wrapper for log_parse.h

   Some context:

   - log_parse.h is a parser (actually just a lexer) for the ad-hoc
     log file format used in uc_tools and derivative firmware.  it
     supports textual logs + escape for short (127 byte) time-stamped
     binary messages.  it works by feeding it log fragments, and it
     will invoke one of a collection of callbacks per log message
     type.

   - this file uses the log_parse.h state machine and provides
     callbacks that convert the different message types to strings, to
     emulate the old string-only log format.

   - this setup can then be used to gradually move firmware to binary
     logging, for performance issues (currently e.g. getting buffer
     overflows for high frame rates), and to start removing literal
     strings from the binary as they take up a lot of space

   - later, this could be extended by other analyzer state machines
     instead of simple conversion to string.  e.g. for status panel.

*/

/* What I want:
   - set up test system to insert a trigger
   - let log system scan for that trigger
   So what is most important is fast scanning, so do not do that in Lua.
*/



// FIXME: For some reason this (which is already in os_linux.h) is not
// enough to expose mremap.
// #define _GNU_SOURCE
// Workaround:
#define __USE_GNU
#include <sys/mman.h>

#include "log_parse.h"
#include "mmap_file.h"

// Lua headers are included in this file.
#include "lua_tools.h"

/* Wrap mmap_file.h read-only memory-mapped file.
   This is for handling large files. */
#define T_LOG_FILE  "uc_tools.log_file"
struct log_file_ud {
    struct mmap_file file;
    lua_State *L;
};
static int gc_log_file(lua_State *L) {
    return 0;
}
static struct log_file_ud *push_log_file(lua_State *L, const char *filename) {
    struct log_file_ud *ud = lua_newuserdata(L, sizeof(*ud));
    ASSERT(ud);
    memset(ud,0,sizeof(*ud));
    mmap_file_open_ro(&ud->file, filename);
    ASSERT(ud->file.buf);
    luaL_getmetatable(L, T_LOG_FILE);
    lua_setmetatable(L, -2);
    return ud;
}
static int cmd_new_log_file(lua_State *L) {
    const uint8_t *filename = (const uint8_t *)lua_tostring(L, -1);
    ASSERT(filename);
    push_log_file(L, (const char*)filename);
    return 1;
}
static struct log_file_ud *L_log_file(lua_State *L, int index) {
    ASSERT(luaL_checkudata(L, index, T_LOG_FILE));
    struct log_file_ud *ud = lua_touserdata(L, index);
    return ud;
}


/* Wrap log_parse.h iterator. */
#define T_LOG_PARSE "uc_tools.log_parse"
struct log_parse_ud {
    struct log_parse s;
    lua_State *L;
    uint32_t nb_rv;
    log_parse_status_t mode;
    uintptr_t offset;
};
static void write_hex_u32(uint8_t *buf, uint32_t val, uint32_t nb) {
    uint8_t hex[] = "0123456789abcdef";
    for (int i=0; i<nb; i++) {
        buf[nb-i-1] = hex[0xF&(val >> (4*i))];
    }
}
static log_parse_status_t to_string_line_cb(
    struct log_parse *s, uint32_t ts_dummy,
    const uint8_t *line, uintptr_t len)
{
    ASSERT(ts_dummy == 0);
    struct log_parse_ud *ud = (void*)s;
    char c_line[len+1];
    memcpy(c_line, line, len);
    c_line[len] = 0;
    // LOG("LINE:%s\n", c_line);
    lua_pushlstring(ud->L, (const char*)line, len);
    ud->nb_rv++;
    return ud->mode;
}
static log_parse_status_t to_string_ts_line_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len)
{
    struct log_parse_ud *ud = (void*)s;
    uint8_t out[len + 9];
    write_hex_u32(out, ts, 8);
    out[8] = ' ';
    memcpy(out+9, line, len);
    lua_pushlstring(ud->L, (const char*)out, sizeof(out));
    ud->nb_rv++;
    return ud->mode;
}
static log_parse_status_t to_string_bin_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len)
{
    struct log_parse_ud *ud = (void*)s;
    uint8_t out[len*3 + 9];
    write_hex_u32(out, ts, 8);
    for (int i=0; i<len; i++) {
        uint8_t *w = out + 8 + i*3;
        w[0] = ' ';
        write_hex_u32(w+1, line[i], 2);
    }
    out[sizeof(out)-1] = '\n';
    lua_pushlstring(ud->L, (const char*)out, sizeof(out));
    ud->nb_rv++;
    return ud->mode;
}
static struct log_parse_ud *push_log_parse(lua_State *L) {
    struct log_parse_ud *ud = lua_newuserdata(L, sizeof(*ud));
    ASSERT(ud);
    memset(ud,0,sizeof(*ud));
    log_parse_init(&ud->s);
    luaL_getmetatable(L, T_LOG_PARSE);
    lua_setmetatable(L, -2);
    return ud;
}
static int gc_log_parse(lua_State *L) {
    return 0;
}
static int cmd_new_log_parse(lua_State *L) {
    push_log_parse(L);
    return 1;
}
static struct log_parse_ud *L_log_parse(lua_State *L, int index) {
    ASSERT(luaL_checkudata(L, index, T_LOG_PARSE));
    struct log_parse_ud *ud = lua_touserdata(L, index);
    return ud;
}

/* Approximation of the semantics of Scheme's parameterize.
   These values are only valid during the extent of the call. */
static void log_parse_parameterize(lua_State *L, struct log_parse_ud *ud) {
    ud->L = L;
    ud->nb_rv = 0;
    ud->s.line_cb    = to_string_line_cb;
    ud->s.ts_line_cb = to_string_ts_line_cb;
    ud->s.ts_bin_cb  = to_string_bin_cb;
    ud->mode = LOG_PARSE_STATUS_CONTINUE;
}

/* Push a string fragment into the state machine.  For each complete
   log line or binary message a callback is invoked, which pushes a
   string to the Lua stack. */
static int cmd_to_string_mv(lua_State *L) {
    struct log_parse_ud *ud = L_log_parse(L, -2);
    const uint8_t *data = (const uint8_t *)lua_tostring(L, -1);
    ASSERT(data);
    size_t len = lua_strlen(L, -1);
    log_parse_parameterize(L, ud);
    log_parse_write(&ud->s, data, len);
    return ud->nb_rv;
}

/* Combine parser and mmap file to create an interator. */
static int cmd_next(lua_State *L) {
    struct log_file_ud *ud_file   = L_log_file(L, -1);
    struct log_parse_ud *ud_parse = L_log_parse(L, -2);
    log_parse_parameterize(L, ud_parse);
    ud_parse->mode = LOG_PARSE_STATUS_YIELD;
    ud_parse->s.in     = ud_file->file.buf  + ud_parse->offset;
    ud_parse->s.in_len = ud_file->file.size - ud_parse->offset;
    log_parse_continue(&ud_parse->s);
    ud_parse->offset = ud_parse->s.in - (const uint8_t*)ud_file->file.buf;
    return ud_parse->nb_rv;
}
static int cmd_reset(lua_State *L) {
    struct log_parse_ud *ud_parse = L_log_parse(L, -1);
    ud_parse->offset = 0;
    return 0;
}


/* init */
static int cmd_name(lua_State *L) {
    const char *str = "log_parse_lua51";
    lua_pushstring(L, str);
    return 1;
}
#define FUN(_name) { \
    lua_pushcfunction (L, cmd_##_name); \
    lua_setfield (L, -2, #_name); \
    }
int luaopen_log_parse_lua51 (lua_State *L) {
    new_lua_metatable(L, T_LOG_PARSE,  gc_log_parse);
    new_lua_metatable(L, T_LOG_FILE,   gc_log_file);
    lua_newtable(L);
    FUN(name);
    FUN(new_log_parse);
    FUN(new_log_file);
    FUN(to_string_mv);
    FUN(reset);
    FUN(next);
    return 1;
#undef FUN
}



