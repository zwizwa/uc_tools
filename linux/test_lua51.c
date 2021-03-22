#include <unistd.h>
#include "lua.h"
#include "lauxlib.h"

int luapi_wait_msec (lua_State *L) {
    int msec = (int) luaL_checknumber (L, -1);
    lua_pop (L, 1);
    usleep (msec * 1000);
    return 0;
}

/* ... more functions ... */


int luaopen_luapi (lua_State *L) {
    lua_newtable(L);
    lua_pushcfunction (L, luapi_wait_msec);
    lua_setfield (L, -2, "wait_msec");
    /* ... more push operations ... */
    return 1;
}
