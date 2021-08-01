#include <unistd.h>

// Lua5.1 wrapper for elfutils libelf/libdw.

// Very ad-hoc, implements minimal functionality needed for
// instrumentation.  Note that there are Rust wrappers (of course),
// which are probably a better starting point for an analysis tool,
// but I currently do not want that dependency.  This is written for
// monitoring an embedded system that already has a Lua based
// framework around it.


// Libelf requires some knowledge of the structure of ELF files.
// https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

// Build against 5.1 for best compatibility with both Lua and LuaJIT
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>

// elfutils
#include <libelf.h>
#include <elfutils/libdw.h>
#include <dwarf.h>

// uc_tools
#include "macros.h"

// DWARF is a tree of Debugging Information Entries (DIEs) per
// Compilation Unit (cu).


// apt-get install liblua5.1-0-dev libdw-dev libelf-dev

// To link this on Linux, it doesn't seem necessary to resolve all the
// symbols at .so link time.  Lua binary provides them on ldopen.

// More links:
// https://sourceware.org/elfutils/ (project page)
// https://github.com/cuviper/elfutils (github mirror)
// https://simonkagstrom.livejournal.com/51001.html



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
    Elf_Scn *symtab_scn; // symbol section
    size_t strtab_ndx;
    Dwarf *dwarf;
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
static const lua_Number L_number(lua_State *L, int index) {
    ASSERT(lua_isnumber(L, index));
    lua_Number n = lua_tonumber(L, index);
    return n;
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
    // LOG("opening %s\n", filename);
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

    size_t ndx = 0;
    Elf_Scn *scn;
    while((scn = elf_getscn(elf, ndx))) {

        GElf_Shdr shdr = {};
        gelf_getshdr(scn, &shdr);

        /* Section names are in the string section with index
           e_shstrndx linked from the ELF header. */
        const char *name = elf_strptr(ud->elf, ud->ehdr.e_shstrndx, shdr.sh_name);
        ASSERT(name);

        // LOG("%2d %s (0x%x)\n", i, name, shdr.sh_type);

        /* Cache some section information as index or pointer,
           depending on how it is used later. */
        if      (!strcmp(".strtab", name)) { ud->strtab_ndx = ndx; }
        else if (!strcmp(".symtab", name)) { ud->symtab_scn = scn; }

        ndx++;
    }
    ASSERT(ud->symtab_scn);
    ASSERT(ud->strtab_ndx);

    // Initialize DWARF index
    ASSERT(ud->dwarf = dwarf_begin_elf(elf, DWARF_C_READ, NULL));

    // Add a metatable...
    luaL_getmetatable(L, "elf");
    lua_setmetatable(L, -2);
    // ... so we can later use this call to type-check.
    ASSERT(luaL_checkudata(L, -1, "elf"));

    return 1;
}

/* Another attempt at simpler C iterators. */
#define FOR_ITER(iter_name, iter_inst, ...)                             \
    for(iter_name##_t iter_inst = iter_name##_new(__VA_ARGS__) ;        \
        iter_name##_valid(&iter_inst);                                  \
        iter_name##_next(&iter_inst))

typedef struct {
    const char *name;
    Elf_Data *data_sym;
    size_t ndx_sym;
    GElf_Sym sym;
    struct elf_userdata *ud;
} sym_iter_t;
static inline void sym_iter_next(sym_iter_t *i) {
    i->name = NULL;
    if (gelf_getsym(i->data_sym, i->ndx_sym, &i->sym) == NULL) return;
    i->name = elf_strptr(i->ud->elf, i->ud->strtab_ndx, i->sym.st_name);
    ASSERT(i->name);
    i->ndx_sym++;
}
static inline sym_iter_t sym_iter_new(struct elf_userdata *ud) {
    sym_iter_t i = { .ud = ud };
    ASSERT(i.data_sym = elf_getdata(ud->symtab_scn, 0));
    sym_iter_next(&i);
    return i;
}
static inline int sym_iter_valid(sym_iter_t *i) {
    return !!i->name;
}

/* Note that we're embedding into floating point!.  That's ok for
   32bit, but won't work for all 64bit addresses. */
static int cmd_sym2addr(lua_State *L) {
    struct elf_userdata *ud = L_elf(L, -2);
    const char *name = L_string(L, -1);
    FOR_ITER(sym_iter, i, ud) {
        // LOG("%08x %s\n", i.sym.st_value, i.name);
        if (!strcmp(i.name, name)) {
            lua_pushnumber(L, i.sym.st_value);
            return 1;
        }
    }
    return 0;
}
static int cmd_addr2sym(lua_State *L) {
    struct elf_userdata *ud = L_elf(L, -2);
    lua_Number addr = L_number(L, -1);
    FOR_ITER(sym_iter, i, ud) {
        // LOG("%08x %s\n", i.sym.st_value, i.name);
        typeof(i.sym.st_value) addr1 = addr;
        if (addr1 == i.sym.st_value) {
            lua_pushstring(L, i.name);
            return 1;
        }
    }
    return 0;
}
// Walk DWARF

static void log_indent(int level) {
    for (int i=0; i<level; i++) { LOG(" "); }
}
// This is the list that occurs in current CM3 image.
// I did the tag number -> name mapping using /usr/include/llvm-3.8/llvm/Support/Dwarf.def
// But it is also in /usr/include/dwarf.h from libdw-dev

#define FOR_DW_TAG(m)                           \
    m(compile_unit)                             \
    m(base_type)                                \
    m(typedef)                                  \
    m(structure_type)                           \
    m(member)                                   \
    m(subprogram)                               \
    m(formal_parameter)                         \
    m(volatile_type)                            \
    m(enumerator)                               \
    m(enumeration_type)                         \
    m(pointer_type)                             \
    m(const_type)                               \
    m(subroutine_type)                          \
    m(array_type)                               \
    m(subrange_type)                            \
    m(union_type)                               \
    m(variable)                                 \
    m(lexical_block)                            \
    m(label)                                    \
    m(inlined_subroutine)                       \
    m(unspecified_parameters)                   \
    m(GNU_call_site)                            \
    m(GNU_call_site_parameter)                  \

#define FOR_DW_AT(m)                            \
    m(producer)                                 \
    m(language)                                 \
    m(name)                                     \
    m(comp_dir)                                 \
    m(ranges)                                   \
    m(low_pc)                                   \
    m(high_pc)                                  \
    m(stmt_list)                                \
    m(byte_size)                                \
    m(encoding)                                 \
    m(location)                                 \
    m(decl_file)                                \
    m(decl_line)                                \
    m(type)                                     \
    m(data_member_location)                     \
    m(sibling)                                  \
    m(const_value)                              \
    m(inline)                                   \
    m(prototyped)                               \
    m(upper_bound)                              \
    m(abstract_origin)                          \
    m(artificial)                               \
    m(declaration)                              \
    m(external)                                 \
    m(frame_base)                               \
    m(entry_pc)                                 \
    m(call_file)                                \
    m(call_line)                                \
    m(GNU_call_site_value)                      \
    m(GNU_call_site_target)                     \
    m(GNU_tail_call)                            \
    m(GNU_all_tail_call_sites)                  \
    m(GNU_all_call_sites)                       \
    m(GNU_macros)                               \

#define CASE_LOG_DW(name) \
    case DW_TAG_##name: LOG(#name "\n"); break;
#define CASE_LOG_AT(name) \
    case DW_AT_##name: LOG(#name "\n"); break;

int walk_attrs(Dwarf_Attribute *attr, void *ctx) {
    switch(attr->code) {
        FOR_DW_AT(CASE_LOG_AT)
    default:
        LOG("attr 0x%x 0x%x\n", attr->code, attr->form);
    }
    //attr->valp
    return DWARF_CB_OK;
}

static void walk_die_tree(Dwarf_Die *die, int level) {
    Dwarf_Die die_ = {};
    for(;;) {
        int tag = dwarf_tag(die);
        log_indent(level);

        /* name */
        const char *name = dwarf_diename(die);
        if (!name) name = "";
        LOG("%s:", name);

        /* type */
        switch(tag) {
        FOR_DW_TAG(CASE_LOG_DW)
        default:
            LOG("(0x%x)\n", tag); // see dwarf.h
        }

        /* attributes */
        // see also dwarf_attr, search by name
        dwarf_getattrs(die, walk_attrs, NULL, 0);


        /* children */
        Dwarf_Die child = {};
        if (!dwarf_child(die, &child)) {
            walk_die_tree(&child, level+1);
        }

        /* siblings */
        if (dwarf_siblingof(die, &die_) == 0) {
            die = &die_;
            continue;
        }
        break;
    }
}

static int cmd_doodle(lua_State *L) {
    struct elf_userdata *ud = L_elf(L, -1);
    Dwarf_Off off=0, next_off=0, abbrev_offset=0;
    size_t header_size=0;
    uint8_t address_size=0, offset_size=0;
    while(dwarf_nextcu(ud->dwarf, off,
                       &next_off, &header_size, &abbrev_offset,
                       &address_size, &offset_size) == 0) {
        Dwarf_Die cu_die;
        ASSERT(dwarf_offdie(ud->dwarf, off + header_size, &cu_die));
        walk_die_tree(&cu_die, 0);
        off = next_off;
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
    CMD(sym2addr);
    CMD(addr2sym);
    CMD(doodle);


#undef CMD
    return 1;
}
