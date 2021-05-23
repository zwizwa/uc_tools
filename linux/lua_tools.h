#ifndef LUA_TOOLS_H
#define LUA_TOOLS_H

// see test_lua51.c for comments on Lua C library API
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>

#include "macros.h"

static inline const char* assert_lua_tostring(lua_State *L, int index) {
    ASSERT(index < 0);
    if (index < 0) {
        ASSERT(lua_gettop(L) >= -index);
    }
    ASSERT(lua_isstring(L, index));
    return lua_tostring(L, index);
}

#endif
