#include <unistd.h>

// Lua5.1 wrapper for elfutils libelf/libdw.
// Very ad-hoc, implements minimal functionality needed for debugging.

// Libelf requires some knowledge of the structure of ELF files.
// https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

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

/* This is the object passed around in Lua. */
struct elf_userdata {
    int fd;
    Elf *elf;
    // translated copy of the ELF header
    GElf_Ehdr ehdr;
};

/* Argument accessors are unforgiving atm, they trigger abort on type
   error.  FIXME: Probably best to raise a Lua error instead. */
static struct elf_userdata *L_elf(lua_State *L, int index) {
    ASSERT(luaL_checkudata(L, index, "elf"));
    struct elf_userdata *ud = lua_touserdata(L, index);
    return ud;
}
static const char *L_string(lua_State *L, int index) {
    ASSERT(lua_isstring(L, index));
    const char *c = lua_tostring(L, index);
    ASSERT(c);
    return c;
}

static void elf_error(void) {
    int e = elf_errno();
    const char *m = elf_errmsg(e);
    ERROR("elf_errmsg: %s, elf_errno: %d\n", m, e);
}


static int cmd_open(lua_State *L) {
    const char *filename = L_string(L, -1);
    elf_version(EV_NONE);
    ASSERT(EV_NONE != elf_version(EV_CURRENT));
    int fd;
    LOG("opening %s\n", filename);
    ASSERT_ERRNO(fd = open(filename, O_RDONLY));
    // LOG("fd = %d\n", fd);
    Elf_Cmd cmd = ELF_C_READ;
    Elf *elf = elf_begin(fd, cmd, NULL);
    // LOG("elf = %p\n", elf);
    if (!elf) elf_error();
    ASSERT(elf);
    struct elf_userdata *ud = lua_newuserdata(L, sizeof(*ud));
    ASSERT(ud);
    memset(ud,0,sizeof(*ud));

    ud->elf = elf;
    ud->fd = fd;

    // Get translated copy of the ELF file header
    ASSERT(gelf_getehdr(elf, &ud->ehdr));
    ASSERT(ud->ehdr.e_shstrndx);


#if 1

    size_t i = 0;
    Elf_Scn *scn;
    while((scn = elf_getscn(elf, i))) {

        GElf_Shdr shdr = {};
        gelf_getshdr(scn, &shdr);

        const char *name = elf_strptr(ud->elf, ud->ehdr.e_shstrndx, shdr.sh_name);
        ASSERT(name);


        // defines are in elf.h
        // just handling things i find in practice
        switch(shdr.sh_type) {
        case SHT_PROGBITS: /* 1 */
        case SHT_SYMTAB: /* 2 */
        case SHT_STRTAB: /* 3 */
        case SHT_RELA: /* 4 */
        case SHT_DYNAMIC: /* 6 */
        case SHT_NOTE: /* 7 */
        case SHT_NOBITS: /* 8 */
        case SHT_DYNSYM: /* 11 */
        case SHT_INIT_ARRAY: /* 14 */
        case SHT_FINI_ARRAY: /* 15 */
        case SHT_GNU_HASH: /* 0x6ffffff6 */
        case SHT_GNU_verneed: /* 0x6ffffffe */
        case SHT_GNU_versym:  /* 0x6fffffff */
        default: {
            LOG("%2d %s (0x%x)\n",
                i,
                name,
                shdr.sh_type);
            break;
        }
        }

        i++;
    }
#endif

    // Add a metatable...
    luaL_getmetatable(L, "elf");
    lua_setmetatable(L, -2);
    // ... so we can later use this call to type-check.
    ASSERT(luaL_checkudata(L, -1, "elf"));

    return 1;
}

static int cmd_test(lua_State *L) {
    struct elf_userdata *ud = L_elf(L, -1);
    (void)ud;
    return 0;
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
    CMD(test);
#undef CMD
    return 1;
}
