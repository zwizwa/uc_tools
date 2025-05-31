/* Wrapper for the minimal C webserver code. */

#include "macros.h"
#include <stdint.h>


/* The original websocket.h code was written for struct blocking_io In
   websocket_pure.h this has been abstracted out further, and
   parameterized by these macros, so we can create a try/abort
   implementation instead. */
#define WS_PURE
#define WS_READ(io, buf, len) { if(0) goto error_exit; ws_buf_read(io, buf, len); }
#define WS_WRITE(io, buf, len) {(void)io; (void)buf; (void)len; if(0) goto error_exit;}
#define WS_ERR_T uint32_t
#define WS_OK 0
#define WS_IO_T struct ws_io
#define WS_LOG_ERROR LOG
struct ws_io {
};
WS_ERR_T ws_buf_read(struct ws_io *ws_io, uint8_t *buf, size_t len) {
    return WS_OK;
}
#include "websocket.h"


#include "lua_tools.h"

static const char *string_L(lua_State *L, int index, size_t *len) {
    ASSERT(lua_isstring(L, index));
    return lua_tolstring(L, index, len);
}

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

int luaopen_webserver_lua51(lua_State *L) {
    lua_newtable(L);
    DEF_CFUN(websocket_sha1);
    return 1;
}
