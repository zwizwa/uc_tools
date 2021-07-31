#include <unistd.h>

// Build against 5.1 for best compatibility with both Lua and LuaJIT
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>

// elfutils
#include <libelf.h>
#include <elfutils/libdw.h>

// uc_tools
#include "macros.h"

// apt-get install liblua5.1-0-dev libdw-dev libelf-dev

// To link this on Linux, it doesn't seem necessary to resolve all the
// symbols at .so link time.  Lua binary provides them on ldopen.



static int cmd_name(lua_State *L) {
    const char *str = "elfutils_lua51";
    lua_pushstring(L, str);
    return 1;
}



static int cmd_open(lua_State *L) {
    ASSERT(lua_isstring(L, -1));
    const char *filename = lua_tostring(L, -1);
    ASSERT(filename);
    elf_version(EV_NONE);
    ASSERT(EV_NONE != elf_version(EV_CURRENT));
    int fd;
    LOG("opening %s\n", filename);
    ASSERT_ERRNO(fd = open(filename, O_RDONLY));
    // LOG("fd = %d\n", fd);
    Elf_Cmd cmd = ELF_C_READ;
    Elf *elf = elf_begin(fd, cmd, NULL);
    // LOG("elf = %p\n", elf);
    if (!elf) {
        int e = elf_errno();
        const char *m = elf_errmsg(e);
        ERROR("elf_errmsg: %s, elf_errno: %d\n", m, e);
    }
    ASSERT(elf);
    void **pelf = lua_newuserdata(L, sizeof(elf));
    *pelf = elf;
    // Add a metatable...
    luaL_getmetatable(L, "elf");
    lua_setmetatable(L, -2);
    // ... so we can later use this call to type-check.
    ASSERT(luaL_checkudata(L, -1, "elf"));

    return 1;
}


int luaopen_elfutils_lua51 (lua_State *L) {
    // FIXME: Add __gc method, maybe also some __name style method to allow pretty printing?
    luaL_newmetatable(L, "elf");

    lua_newtable(L);
#define CMD(_name) { \
    lua_pushcfunction (L, cmd_##_name); \
    lua_setfield (L, -2, #_name); \
    }
    CMD(name);
    CMD(open);
#undef CMD
    return 1;
}
