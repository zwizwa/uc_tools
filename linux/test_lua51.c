#include <unistd.h>

// Build against 5.1 for best compatibility with both Lua and LuaJIT
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>

// apt-get install liblua5.1-0-dev

// To link this on Linux, it doesn't seem necessary to resolve all the
// symbols at .so link time.  Going to assume this happens when Lua
// ldopens the library.


int luapi_wait_msec (lua_State *L) {
    int msec = (int) luaL_checknumber (L, -1);
    lua_pop (L, 1);
    usleep (msec * 1000);
    return 0;
}

/* ... more functions ... */


int luaopen_test_lua51 (lua_State *L) {
    lua_newtable(L);
    lua_pushcfunction (L, luapi_wait_msec);
    lua_setfield (L, -2, "wait_msec");
    /* ... more push operations ... */
    return 1;
}
