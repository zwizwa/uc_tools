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
    GElf_Ehdr ehdr;   // translated copy of the ELF header
    Elf_Scn *scn_sym; // symbol section
    size_t strtab_ndx;
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

        /* Section names are in e_shstrndx which we've resolved. */
        const char *name = elf_strptr(ud->elf, ud->ehdr.e_shstrndx, shdr.sh_name);
        ASSERT(name);

        LOG("%2d %s (0x%x)\n", i, name, shdr.sh_type);

        if (!strcmp(".strtab", name)) {
            /* The string table used in the symbol table. */
            ud->strtab_ndx = i;
        }
        if (!strcmp(".symtab", name)) {
            /* Symbol table. */
            ud->scn_sym = scn;
        }

        i++;
    }
#endif

    ASSERT(ud->scn_sym);
    ASSERT(ud->strtab_ndx);

    // Add a metatable...
    luaL_getmetatable(L, "elf");
    lua_setmetatable(L, -2);
    // ... so we can later use this call to type-check.
    ASSERT(luaL_checkudata(L, -1, "elf"));

    return 1;
}

/* Another attempt at simpler C iterators. */
#define FOR_ITER(iter_name, iter_inst, ...)                             \
    for(iter_name##_type iter_inst = iter_name##_new(__VA_ARGS__) ;     \
        iter_name##_valid(&iter_inst);                                  \
        iter_name##_next(&iter_inst))

typedef struct {
    const char *name;
    Elf_Data *data_sym;
    size_t ndx_sym;
    GElf_Sym sym;
    struct elf_userdata *ud;
} sym_iter_type;
static inline void sym_iter_next(sym_iter_t *i) {
    i->name = NULL;
    if (gelf_getsym(i->data_sym, i->ndx_sym, &i->sym) == NULL) return;
    i->name = elf_strptr(i->ud->elf, i->ud->strtab_ndx, i->sym.st_name);
    ASSERT(i->name);
    i->ndx_sym++;
}
static inline sym_iter_t sym_iter_new(struct elf_userdata *ud) {
    sym_iter_t i = { .ud = ud };
    ASSERT(i.data_sym = elf_getdata(ud->scn_sym, 0));
    sym_iter_next(&i);
    return i;
}
static inline int sym_iter_valid(sym_iter_t *i) {
    return !!i->name;
}

static int cmd_getsym(lua_State *L) {
    struct elf_userdata *ud = L_elf(L, -2);
    const char *sym_name = L_string(L, -1);
    (void)sym_name;
    FOR_ITER(sym_iter, i, ud) {
        LOG("%08x %s\n", i.sym.st_value, i.name);
    }
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
    CMD(getsym);
    
#undef CMD
    return 1;
}
