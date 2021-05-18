/* A wrapper around the logic analyzer server. See also la_ws.c

   The way this is hooked up is very pragmatic: the point for this is
   to be included in a test system that is written in Lua, and I want
   to make sure that not a whole lot of time is lost in marshalling
   data back and forth, so principle one:

   1. Interface between analyzer and Lua is as a Lua C module, such
      that we can just push lua tables and strings from C.

   2. Anything else is just bolted on top, such as the web interface.

   3. Use the mod_ system

*/

#include <unistd.h>

// Build against 5.1 for best compatibility with both Lua and LuaJIT
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>

// apt-get install liblua5.1-0-dev

// To link this on Linux, it doesn't seem necessary to resolve all the
// symbols at .so link time.  Lua binary provides them on ldopen.

static int name(lua_State *L) {
    const char *str = "la_lua51";
    lua_pushstring(L, str);
    return 1;
}


int luaopen_la_lua51 (lua_State *L) {
    lua_newtable(L);
#define FUN(_name) { \
    lua_pushcfunction (L, _name); \
    lua_setfield (L, -2, #_name); \
    }
    FUN(name);
#undef FUN
    return 1;
}
