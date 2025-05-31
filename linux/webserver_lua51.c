/* Wrapper for the minimal C webserver code. */

#include "macros.h"
#include <stdint.h>

/* The websocket.h header is parameterized by osal.  Note that we do
   not use sockets directly in this Lua interface.  This is just to
   avoid having to split the webserver code into pure functions and
   osal functions. */
#include "os_linux.h"
#include "os_tcp_berkeley.h"

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
