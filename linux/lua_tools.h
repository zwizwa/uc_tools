#ifndef LUA_TOOLS_H
#define LUA_TOOLS_H

// Build against 5.1 for best compatibility with both Lua and LuaJIT
// #ifdef __x86_64__
#if 0
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>
#else
#include "lua.h"
#include "lauxlib.h"
#endif

// apt-get install liblua5.1-0-dev


#include "macros.h"

static inline const char* assert_lua_tostring(lua_State *L, int index) {
    ASSERT(index < 0);
    if (index < 0) {
        ASSERT(lua_gettop(L) >= -index);
    }
    ASSERT(lua_isstring(L, index));
    return lua_tostring(L, index);
}


static inline const lua_Number L_number(lua_State *L, int index) {
    ASSERT(lua_isnumber(L, index));
    lua_Number n = lua_tonumber(L, index);
    return n;
}

static inline void new_lua_metatable(lua_State *L, const char *t_name, int (*gc)(lua_State *)) {
    luaL_newmetatable(L, t_name);
    luaL_getmetatable(L, t_name);
    lua_pushstring(L, t_name); lua_setfield(L, -2, "__name");
    lua_pushcfunction(L, gc);  lua_setfield(L, -2, "__gc");
    lua_pop(L, -1);
}



#endif
