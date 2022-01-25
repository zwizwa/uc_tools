#include <unistd.h>
#include <stdint.h>

/* Lua wrapper for log_parse.h

   Some context:

   - log_parse.h is a parser (scanner) for the ad-hoc log file format
     used in uc_tools and derivative firmware.  it supports textual
     logs + escape for short (127 byte) time-stamped binary messages.
     it works by feeding it log fragments, and it will invoke one of a
     collection of callbacks per log message type.

   - this file uses the log_parse.h state machine and provides
     callbacks that convert the different message types to strings, to
     emulate the old string-only log format.

   - this setup can then be used to gradually move firmware to binary
     logging, for performance issues (currently e.g. getting buffer
     overflows for high frame rates), and to start removing literal
     strings from the binary as they take up a lot of space

   - later, this could be extended by other analyzer state machines
     instead of simple conversion to string.  e.g. for status panel.

   - this also acts as an example/experiment on how to solve this KIND
     of problem: represent an iterator in lua, parameterize the
     element representation.

*/

/* What I want:
   - set up test system to insert a trigger
   - let log system scan for that trigger
   So what is most important is fast scanning, so do not do that in Lua.
*/


/* Output types:

   string     Single string containing hex timestamp.
   bin        As string, converting ts to hex but not the binary message.
   index      Generate time stamp number + spans instead of strings.
   ts_string  Timestamp number + string logline
   ts_raw     As ts_string, but don't convert log message to hex.
*/



#include "log_parse_lua51.h"

/* Wrap mmap_file.h read-only memory-mapped file.
   This is for handling large files. */
#define T_LOG_FILE  "uc_tools.log_file"
static int gc_log_file(lua_State *L) {
    LOG("FIXME: gc_log_file()\n");
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

/* Different output records.
   The names have gotten a bit arbitrary.
   Maybe refactor?  Otoh this is almost done, but at least rename these.
   The eyesore is bin which is used in several places.
*/

static void write_hex_u32(uint8_t *buf, uint32_t val, uint32_t nb) {
    uint8_t hex[] = "0123456789abcdef";
    for (int i=0; i<nb; i++) {
        buf[nb-i-1] = hex[0xF&(val >> (4*i))];
    }
}
static uintptr_t mmap_file_offset(struct log_parse_ud *ud) {
    /* log_parser.h will save ->in in ->in_mark before invoking the
       callbacks. If ->in is stable, e.g. for mmap files as we are
       using it, we can compute the file offset this way. */
    ASSERT(ud->ud_file);
    const uint8_t *start = ud->ud_file->file.buf;
    return ud->s.in_mark - start;
}

/* The callbacks are named <cb_name>_<out_type_name>_cb.
   These all receive log_parse_ud as context, which has Lua context.
   Lua result values are passed onto the stack + nb_rv is marked. 
*/

static log_parse_status_t ts_line_index_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len)
{
    struct log_parse_ud *ud = (void*)s;
    lua_pushnumber(ud->L, ts);
    lua_pushnumber(ud->L, mmap_file_offset(ud));
    lua_pushnumber(ud->L, len);
    ud->nb_rv += 3;
    return ud->mode;
}

static log_parse_status_t ts_line_ts_string_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len)
{
    struct log_parse_ud *ud = (void*)s;
    lua_pushnumber(ud->L, ts);
    lua_pushlstring(ud->L, (const char*)line, len);
    ud->nb_rv += 2;
    return ud->mode;
}

#define ts_line_ts_raw_cb ts_line_ts_string_cb // REUSE

static log_parse_status_t ts_line_string_cb(
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
#define ts_line_bin_cb ts_line_string_cb // REUSE

static log_parse_status_t ts_bin_index_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len)
{
    struct log_parse_ud *ud = (void*)s;
    lua_pushnumber(ud->L, ts);
    lua_pushnumber(ud->L, mmap_file_offset(ud));
    lua_pushnumber(ud->L, len);
    ud->nb_rv += 3;
    return ud->mode;
}

static log_parse_status_t ts_bin_ts_raw_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len)
{
    struct log_parse_ud *ud = (void*)s;
    /* Same as ts_string, but don't convert to hex, and leave
       extra 'true' argument to distinguish.  */
    lua_pushnumber(ud->L, ts);
    lua_pushlstring(ud->L, (const char*)line, len);
    lua_pushboolean(ud->L, 1);
    ud->nb_rv += 3;
    return ud->mode;
}

static log_parse_status_t ts_bin_ts_string_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len)
{
    struct log_parse_ud *ud = (void*)s;
    uint8_t out[len*3];
    memset(out,' ',sizeof(out));
    for (int i=0; i<len; i++) {
        uint8_t *w = out + i*3;
        write_hex_u32(w, line[i], 2);
    }
    out[sizeof(out)-1] = '\n';
    lua_pushnumber(ud->L, ts);
    lua_pushlstring(ud->L, (const char*)out, sizeof(out));
    ud->nb_rv += 2;
    return ud->mode;
}

static log_parse_status_t ts_bin_string_cb(
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

static log_parse_status_t ts_bin_bin_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len)
{
    struct log_parse_ud *ud = (void*)s;
    uint8_t out[9 + len];
    write_hex_u32(out, ts, 8);
    out[8] = 0; // string has ' ' here
    memcpy(out+9, line, len);
    lua_pushlstring(ud->L, (const char*)out, sizeof(out));
    ud->nb_rv++;
    return ud->mode;
}


static struct log_parse_ud *push_log_parse(lua_State *L) {
    struct log_parse_ud *ud = lua_newuserdata(L, sizeof(*ud));
    ASSERT(ud);
    memset(ud,0,sizeof(*ud));
    log_parse_init(&ud->s, NULL);
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

/* These values are only valid during the extent of the call. */
static void log_parse_parameterize(
    lua_State *L,
    struct log_parse_ud *ud,
    struct log_parse_cbs *cb,
    int mode)
{
    ud->L = L;
    ud->nb_rv = 0;
    ud->s.cb = cb;
    ud->mode = mode;
}


/* Push a string fragment into the state machine.  For each complete
   log line or binary message a callback is invoked, which pushes a
   string to the Lua stack. */
static int to_thing_mv(lua_State *L, struct log_parse_cbs *cb) {
    struct log_parse_ud *ud = L_log_parse(L, -2);
    const uint8_t *data = (const uint8_t *)lua_tostring(L, -1);
    ASSERT(data);
    size_t len = lua_strlen(L, -1);
    log_parse_parameterize(L, ud, cb, LOG_PARSE_STATUS_CONTINUE);
    log_parse_write(&ud->s, data, len);
    return ud->nb_rv;
}

/* Callback bundles per output type. */
// FIXME: .line is the same as .ts_line here.
// In practice this only happens for junk lines.
// Maybe rename it to "junk" instead?

#define LET_CBS(cbs,tag) \
    struct log_parse_cbs cbs = { \
        .line    =  ts_line_##tag##_cb, \
        .ts_line =  ts_line_##tag##_cb, \
        .ts_bin  =  ts_bin_##tag##_cb, \
    }
LET_CBS(cbs_string,    string);
LET_CBS(cbs_index,     index);
LET_CBS(cbs_ts_string, ts_string);
LET_CBS(cbs_ts_raw,    ts_raw);
LET_CBS(cbs_bin,       bin);




static int cmd_to_string_mv(lua_State *L) {
    return to_thing_mv(L, &cbs_string);
}
static int cmd_to_bin_mv(lua_State *L) {
    return to_thing_mv(L, &cbs_bin);
}

void bind_parse(struct log_parse_ud *ud_parse, struct log_file_ud *ud_file) {
    /* Note that we're creating a tight coupling from parser to file
       object: parser has pointers into mmap file.  For the first
       call, we associate the parser.  On subsequent calls we check
       that the file is still the same.  If not, reset parser. */
    if (!ud_parse->ud_file ||
        ud_parse->ud_file != ud_file) {
        log_parse_init(&ud_parse->s, ud_file->file.buf);
        ud_parse->ud_file = ud_file;
        ud_parse->offset = 0;
    }
}


/* Continue parsing at offset. */
void parse_continue_at_offset(
    struct log_parse_ud *ud_parse, struct log_file_ud *ud_file)
{
    ud_parse->s.in     = ud_file->file.buf  + ud_parse->offset;
    ud_parse->s.in_len = ud_file->file.size - ud_parse->offset;
    log_parse_continue(&ud_parse->s);
    ud_parse->offset = ud_parse->s.in - (const uint8_t*)ud_file->file.buf;
}

/* Yield to parser to provide a single output element of specified type.
   Lua arguments are alwyays the same (parse,file). */
static struct log_parse_ud *log_parse_next_cb(
    lua_State *L, struct log_parse_cbs *cb)
{
    struct log_file_ud *ud_file   = L_log_file(L, -1);
    struct log_parse_ud *ud_parse = L_log_parse(L, -2);
    // FIXME: check that offset is actually inside the file
    bind_parse(ud_parse, ud_file);
    log_parse_parameterize(L, ud_parse, cb, LOG_PARSE_STATUS_YIELD);
    parse_continue_at_offset(ud_parse, ud_file);
    return ud_parse;
}


// FIXME: For now this is hardcoded to single byte prefix search.
// FIXME: Not tested
static log_parse_status_t ts_find_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len)
{
    struct log_parse_ud *ud = (void*)s;
    if ((len > 1) && (line[0]) == ud->prefix) {
        //lua_pushboolean(ud->L, 1);
        //FIXME: the offset of the line would be more useful.
        //Track that separately.
        lua_pushnumber(ud->L, mmap_file_offset(ud));
        ud->nb_rv++;
        return LOG_PARSE_STATUS_YIELD;
    }
    else {
        return LOG_PARSE_STATUS_CONTINUE;
    }
}
static int cmd_wind_prefix(lua_State *L) {
    uint8_t prefix                = L_number(L, -1);
    struct log_file_ud *ud_file   = L_log_file(L, -2);
    struct log_parse_ud *ud_parse = L_log_parse(L, -3);
    bind_parse(ud_parse, ud_file);
    ud_parse->L = L;
    ud_parse->nb_rv = 0;
    struct log_parse_cbs cbs = {
        .line    = ts_find_cb,
        .ts_line = ts_find_cb,
        .ts_bin  = ts_find_cb,
    };
    ud_parse->s.cb = &cbs;
    ud_parse->prefix = prefix;
    parse_continue_at_offset(ud_parse, ud_file);
    return ud_parse->nb_rv;
}


/* Combine parser and mmap file to create an interator. */
static int cmd_next_string(lua_State *L)    { return log_parse_next_cb(L, &cbs_string)->nb_rv; }
static int cmd_next_ts_string(lua_State *L) { return log_parse_next_cb(L, &cbs_ts_string)->nb_rv; }
static int cmd_next_ts_raw(lua_State *L)    { return log_parse_next_cb(L, &cbs_ts_raw)->nb_rv; }
static int cmd_next_bin(lua_State *L)       { return log_parse_next_cb(L, &cbs_bin)->nb_rv; }
/* Same as string, but return timestamp, offset, len instead. */
static int cmd_next_index(lua_State *L)     { return log_parse_next_cb(L, &cbs_index)->nb_rv; }

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
    FUN(to_bin_mv);
    FUN(next_string);
    FUN(next_ts_string);
    FUN(next_ts_raw);
    FUN(next_bin);
    FUN(next_index);
    FUN(wind_prefix);
    return 1;
#undef FUN
}



