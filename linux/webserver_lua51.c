/* Wrapper for the minimal C webserver code. */

#include "macros.h"
#include <stdint.h>


/* The original websocket.h code was written for struct blocking_io In
   websocket_pure.h this has been abstracted out further, and
   parameterized by these macros, so we can create a try/abort
   implementation instead. */
#define WS_PURE
#include "websocket.h"


#include "lua_tools.h"

struct websocket_parse {
    struct ws_buf ws_buf;
    struct lua_State *L;
    int nb_rv;
};
void websocket_parse_init(struct websocket_parse *s) {
    ws_buf_init(&s->ws_buf);
}

#define websocket_parse_T "rdm.websocket_parse"
#define NS(name) CONCAT(websocket_parse,name)
#include "ns_lua_struct.h"
#undef NS


//static const char *string_L(lua_State *L, int index, size_t *len) {
//    ASSERT(lua_isstring(L, index));
//    return lua_tolstring(L, index, len);
//}

static int websocket_sha1_cmd(lua_State *L) {
    uint8_t websocket_sha1[SHA1_BLOCK_SIZE];
    size_t len = 0;
    const char *key = string_L(L, 1, &len);

    ws_write_sha1(key, websocket_sha1);

    int n = base64_length(sizeof(websocket_sha1));
    char bas64_sha1[n+1];
    base64_encode(bas64_sha1, websocket_sha1, sizeof(websocket_sha1));
    bas64_sha1[n] = 0;

    lua_pushstring(L, bas64_sha1);
    return 1;
}

static ws_err_t ws_buf_push(ws_io_t *io, struct ws_message *msg) {
    struct websocket_parse *s = (void*)io;
    lua_pushlstring(s->L, (const char*)msg->buf, msg->len);
    s->nb_rv++;
    return WS_OK;
}

static int websocket_parse_push_chunk_cmd(lua_State *L) {
    struct websocket_parse *s = &websocket_parse_L(L, -2)->base;
    s->L = L;
    s->nb_rv = 0;
    (void)s;
    size_t data_len = 0;
    const uint8_t *data = (const uint8_t*)string_L(L, -1, &data_len);

    ws_err_t err;
#if 1
    err = ws_update(&s->ws_buf, data, data_len, ws_buf_push);
#else
    /* Test the incomplete->abort mechanism. */
    int c = 3;
    err = ws_update(&s->ws_buf, data, c, ws_buf_push);
    err = ws_update(&s->ws_buf, data+c, data_len-c, ws_buf_push);
#endif

    if (err != WS_OK) {
        LOG("WARNING: ws_update error %d\n", err);
    }
    return s->nb_rv;
}

// Encode a message for sending
static int websocket_encode_cmd(lua_State *L) {

    struct websocket_parse *s = &websocket_parse_L(L, 1)->base;
    size_t str_len = 0;
    const char *str = string_L(L, 2, &str_len);
    LOG("websocket_encode: str_len=%d\n", str_len);

    /* The struct ws_message does not have a const buffer pointer.  I
       do not remember if it is written. Probably not, but just be
       safe here and copy. */
    uint8_t buf[str_len]; memcpy(buf, str, str_len);

    /* Connect a temp buffer allocated on stack. */
    struct cbuf out; uint8_t out_buf[1024];
    CBUF_INIT(out);
    s->ws_buf.out = &out;

    struct ws_message msg = {
        .opcode = 1,  // Text Frame
        //.opcode = 2,  // Binary Frame
        .fin = 1,
        .mask = 0,
        .buf = buf,
        .len = str_len,
    };

    int rv;
    if ((rv = setjmp(s->ws_buf.jmp_buf))) {
        LOG("websocket_encode: error=%d\n", rv);
        return 0;
    }
    ws_write_msg_nolock(&s->ws_buf, &msg);


    /* Disconnect temp buffer. */
    s->ws_buf.out = NULL;

    /* Return buffer contents. */
    int nb = cbuf_elements(&out);
    LOG("websocket_encod: nb=%d\n", nb);
    lua_pushlstring(L, (const char*)out.buf, nb);
    return 1;
}



static void new_metatable(lua_State *L, const char *t_name, int (*gc)(lua_State *)) {
    luaL_newmetatable(L, t_name);
    luaL_getmetatable(L, t_name);
    lua_pushstring(L, t_name); lua_setfield(L, -2, "__name");
    lua_pushcfunction(L, gc);  lua_setfield(L, -2, "__gc");
    lua_pop(L, -1);
}
int luaopen_webserver_lua51(lua_State *L) {
    new_metatable(L, websocket_parse_T, websocket_parse_gc);
    lua_newtable(L);
    DEF_CFUN(websocket_sha1);
    DEF_CFUN(websocket_parse_new);
    DEF_CFUN(websocket_parse_push_chunk);
    DEF_CFUN(websocket_encode);
    return 1;
}
