#include <unistd.h>

// Lua5.1 wrapper for elfutils libelf/libdw.
// apt-get install liblua5.1-0-dev libdw-dev libelf-dev

// Very ad-hoc, implements minimal functionality needed for
// instrumentation:

// This was originally developed for a Lua test system that
// orchestrates a network of target devices, and reads out state as
// postcondition.  Getting variable locations from ELF symbol table
// and type information from DWARF makes it possible to avoid target
// accessors for state that is normally hidden.

// Note that there are Rust wrappers (of course), which are probably a
// better starting point for an analysis tool, but I currently do not
// want that dependency.  This is written for monitoring an embedded
// system that already has a Lua based framework around it.

// Libelf requires some knowledge of the structure of ELF files.
// https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

// Build against 5.1 for best compatibility with both Lua and LuaJIT
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>

// elfutils
#include <libelf.h>
#include <elfutils/libdw.h>
#include <dwarf.h>

#include <stdarg.h>

// uc_tools
// macros.h will use this one.  it only works inside the body of a
// function that has L defined.
#define ERROR(...) L_error(L, __VA_ARGS__)
static void L_error(lua_State *L, const char *fmt, ...) {
    char msg[1024];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(msg, sizeof(msg), fmt, ap);
    va_end(ap);
    lua_pushstring(L, msg);
    lua_error(L);
}


#include "macros.h"

// DWARF is a tree of Debugging Information Entries (DIEs) per
// Compilation Unit (cu).

// I found this difficult to understand until it suddenly snapped into
// place.  The main hindrance was the lack of documentation for
// libdw.h but it is possible to piece it together with DWARF4.pdf,
// dwarfdump, and grepping for DW_AT_* in the libdw source.  The code
// in here is sturctured in the following way to keep it simple:
//
// - the ELF is exposed as an object with Elf and Dwarf objects opened
//   and accessible.
//
// - dataststucture walker: CU list -> DIE trie for each. this is used
//   to find a DIE by attribute (e.g. name for now).
//
// - attribute accessors: this is used to translate a DIE attribute to
//   either a primitive Lua value, or a referenced DIE.
//
// - attribute access is by numerical DW_AT_* codes that are
//   duplicated in the Lua source, which is less work than making
//   everything symbolic at the C level.
//
// - all the rest can then be done in Lua


// More links:
// https://sourceware.org/elfutils/ (project page)
// https://github.com/cuviper/elfutils (github mirror)
// https://simonkagstrom.livejournal.com/51001.html
// http://www.dwarfstd.org/doc/DWARF4.pdf
// https://manpages.debian.org/testing/dwarfdump/dwarfdump.1.en.html


static int cmd_name(lua_State *L) {
    const char *str = "elfutils_lua51";
    lua_pushstring(L, str);
    return 1;
}

struct elf_ud {
    int fd;
    Elf *elf;
    GElf_Ehdr ehdr;   // translated copy of the ELF header
    Elf_Scn *symtab_scn; // symbol section
    size_t strtab_ndx;
    Dwarf *dwarf;
};
struct die_ud {
    Dwarf_Die die;
};

#define T_ELF "elfutils.elf"
#define T_DIE "elfutils.die"


/* Argument accessors are unforgiving atm, they trigger abort on type
   error.  FIXME: Probably best to raise a Lua error instead. */
static struct elf_ud *L_elf(lua_State *L, int index) {
    ASSERT(luaL_checkudata(L, index, T_ELF));
    struct elf_ud *ud = lua_touserdata(L, index);
    return ud;
}
static struct die_ud *L_die(lua_State *L, int index) {
    ASSERT(luaL_checkudata(L, index, T_DIE));
    struct die_ud *ud = lua_touserdata(L, index);
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

static void elf_error(lua_State *L) {
    int e = elf_errno();
    const char *m = elf_errmsg(e);
    ERROR("elf_errmsg: %s, elf_errno: %d\n", m, e);
}

static struct die_ud *push_die(lua_State *L) {
    struct die_ud *ud = lua_newuserdata(L, sizeof(*ud));
    ASSERT(ud);
    memset(ud,0,sizeof(*ud));
    luaL_getmetatable(L, T_DIE);
    lua_setmetatable(L, -2);
    return ud;
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
    if (!elf) elf_error(L);

    // FIXME: elf should probably be lightuserdata such that the
    // memory is never freed.  what we really want is refcounting of a
    // shared elf structure (from elf and die references) + a gc
    // method that decreases count and frees if necessary.

    ASSERT(elf);
    struct elf_ud *ud = lua_newuserdata(L, sizeof(*ud));
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
    luaL_getmetatable(L, T_ELF);
    lua_setmetatable(L, -2);
    // ... so we can later use this call to type-check.
    ASSERT(luaL_checkudata(L, -1, T_ELF));

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
    struct elf_ud *ud;
    lua_State *L;
} sym_iter_t;
static inline void sym_iter_next(sym_iter_t *i) {
    lua_State *L = i->L;
    i->name = NULL;
    if (gelf_getsym(i->data_sym, i->ndx_sym, &i->sym) == NULL) return;
    i->name = elf_strptr(i->ud->elf, i->ud->strtab_ndx, i->sym.st_name);
    ASSERT(i->name);
    i->ndx_sym++;
}
static inline sym_iter_t sym_iter_new(struct elf_ud *ud, lua_State *L) {
    sym_iter_t i = { .ud = ud, .L = L };
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
    struct elf_ud *ud = L_elf(L, -2);
    const char *name = L_string(L, -1);
    FOR_ITER(sym_iter, i, ud, L) {
        // LOG("%08x %s\n", i.sym.st_value, i.name);
        if (!strcmp(i.name, name)) {
            lua_pushnumber(L, i.sym.st_value);
            return 1;
        }
    }
    return 0;
}
static int cmd_addr2sym(lua_State *L) {
    struct elf_ud *ud = L_elf(L, -2);
    lua_Number addr = L_number(L, -1);
    FOR_ITER(sym_iter, i, ud, L) {
        // LOG("%08x %s\n", i.sym.st_value, i.name);
        typeof(i.sym.st_value) addr1 = addr;
        if (addr1 == i.sym.st_value) {
            lua_pushstring(L, i.name);
            return 1;
        }
    }
    return 0;
}


// DWARF walker.
//
// It seems simplest to implement a single walker, and instrument it
// with some ad-hoc pugin behavior.

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

static void log_indent(int level) {
    for (int i=0; i<level; i++) { LOG(" "); }
}

struct die_walk;

struct die_walk {
    Dwarf_Die die;
    int verbose;
    lua_State *L;
    int level;
    const char *name;
    int nb_retvals;
};
typedef struct die_walk die_walk_t;

#define WALK_OK 0
#define WALK_ABORT 1
static int walk_die_tree(die_walk_t *s) {
    for(;;) {
        int tag = dwarf_tag(&s->die);
        if (s->verbose) {
            log_indent(s->level);
        }

        /* name */
        const char *name = dwarf_diename(&s->die);
        if (!name) name = "";
        if (s->verbose) LOG("%s:", name);

        /* lookup is implemented as this early abort hack. */
        if (s->name && (!strcmp(name, s->name))) {
            struct die_ud *ud = push_die(s->L);
            ud->die = s->die;
            s->nb_retvals++;
            return WALK_ABORT;
        }

        /* type */
        if (s->verbose) {
            switch(tag) {
                FOR_DW_TAG(CASE_LOG_DW)
            default:
                LOG("(0x%x)\n", tag); // see dwarf.h
            }
        }

        /* attributes are not done during the walk to not complicate
           things.  do it in two steps: find a die using a walk, then
           print attributes. */

        /* children */
        Dwarf_Die parent = s->die;
        if (!dwarf_child(&parent, &s->die)) {
            s->level++;
            if (WALK_ABORT == walk_die_tree(s)) {
                return WALK_ABORT;
            }
            s->level--;
        }
        s->die = parent;

        /* siblings */
        Dwarf_Die sibling;
        if (dwarf_siblingof(&s->die, &sibling) == 0) {
            s->die = sibling;
            continue;
        }
        break;
    }
    return WALK_OK;
}

/* Walk the entire DIE tree, starting at the list of CUs. */
static int die_walk(die_walk_t *s, struct elf_ud *ud) {
    lua_State *L = s->L;
    Dwarf_Off off=0, next_off=0, abbrev_offset=0;
    size_t header_size=0;
    uint8_t address_size=0, offset_size=0;

    while(dwarf_nextcu(ud->dwarf, off,
                       &next_off, &header_size, &abbrev_offset,
                       &address_size, &offset_size) == 0) {
        ASSERT(dwarf_offdie(ud->dwarf, off + header_size, &s->die));
        walk_die_tree(s);
        off = next_off;
    }
    return 0;
}

/* leb128 decode of Dwarf_Block */
static uint32_t block_u32(Dwarf_Block *b) {
    // FIXME: THIS IS WRONG.
    // Currently just a special case.
    // ASSERT(b->length == 5);
    // ASSERT(b->data[0] == 3); // is actually LEB128
    // FIXME: assuming same endianness!
    return *((uint32_t*)(b->data+1));
}

/* Convert die attribute to something usable from Lua.  I could not
   find good documenation for this, so had to grep libdw source for
   DW_FORM_ literals to make sense of things. */
struct log_attr_ctx {
    lua_State *L;
};
static int log_attr(Dwarf_Attribute *attr, void *ctx) {
    struct log_attr_ctx *s = ctx;
    lua_State *L = s->L;
    switch(attr->form) {
    case DW_FORM_strp:
        LOG("%s", dwarf_formstring(attr));
        break;
    case DW_FORM_ref4: {
        Dwarf_Die die_mem;
        ASSERT(dwarf_formref_die(attr, &die_mem));
        LOG("<die>");
        break;
    }
    case DW_FORM_data1: {
        Dwarf_Sword sval;
        ASSERT(0 == dwarf_formsdata(attr, &sval));
        LOG("%d", sval);
        break;
    }
    case DW_FORM_exprloc: {
        Dwarf_Block block;
        ASSERT(0 == dwarf_formblock(attr, &block));
        uint32_t addr = block_u32(&block);
        LOG("0x%08x", addr);
        break;
    }
    default:
        LOG("?");
        break;
    }
    LOG(":0x%x ", attr->form);
    switch(attr->code) {
        FOR_DW_AT(CASE_LOG_AT)
    default:
        LOG("attr 0x%x 0x%x\n", attr->code, attr->form);
    }
    //attr->valp
    return DWARF_CB_OK;
}

static int cmd_sym2die(lua_State *L) {
    struct elf_ud *ud = L_elf(L, -2);
    const char *name = L_string(L, -1);
    die_walk_t s = {
        .L = L,  // for return data and errors
        .name = name // if defined, return only this DIE
    };
    die_walk(&s, ud);
    return s.nb_retvals;
}

static int cmd_die_log(lua_State *L) {
    struct die_ud *die_ud = L_die(L, -1);
    struct log_attr_ctx ctx = { .L = L };
    dwarf_getattrs(&die_ud->die, log_attr, &ctx, 0);
    return DWARF_CB_OK;
}

// To keep the code simple here, we implement only single attribute
// access.  E.g. we don't really need to create a full rendering of
// all DWARF data in Lua, we just need to provide some accessors.  So
// let's write them manually, then maybe later automate using macros.

int cmd_die_attr(lua_State *L) {
    struct die_ud *die_ud = L_die(L, -2);
    unsigned int code     = L_number(L, -1);  // DW_AT_* codes
    Dwarf_Attribute attr;
    ASSERT(dwarf_attr(&die_ud->die, code, &attr));
    switch(attr.form) {
    case DW_FORM_strp:
        lua_pushstring(L, dwarf_formstring(&attr));
        break;
    case DW_FORM_ref4: {
        struct die_ud *ud = push_die(L);
        ASSERT(dwarf_formref_die(&attr, &ud->die));
        break;
    }
    case DW_FORM_exprloc: {
        Dwarf_Block block;
        ASSERT(0 == dwarf_formblock(&attr, &block));
        uint32_t addr = block_u32(&block);
        lua_pushnumber(L, addr);
        break;
    }
    default:
        return 0;
    }
    return 1;
}


// Reify the iteration macro as a Lua data structure.
// To keep it simple, expand it as a C array first.
struct number_table {
    const char *name;
    const unsigned int code;
};
#define DW_AT_FIELD(name) { #name, DW_AT_##name },
const struct number_table DW_AT_table[] = { FOR_DW_AT(DW_AT_FIELD) };

// Then export that as a Lua table.
static int cmd_make_DW_AT(lua_State *L) {
    lua_newtable(L);
    for(int i=0; i<ARRAY_SIZE(DW_AT_table); i++) {
        lua_pushnumber(L, DW_AT_table[i].code);
        lua_setfield(L, -2, DW_AT_table[i].name);
    }
    return 1;
}


static int cmd_doodle(lua_State *L) {
    struct elf_ud *ud = L_elf(L, -1);
    die_walk_t s = { .L = L, .verbose = 1 };
    return die_walk(&s, ud);
}

int luaopen_elfutils_lua51 (lua_State *L) {
    // FIXME: Add __gc method, maybe also some __name style method to allow pretty printing?
    luaL_newmetatable(L, T_ELF);
    luaL_newmetatable(L, T_DIE);

    lua_newtable(L);
#define CMD(_name) { \
    lua_pushcfunction (L, cmd_##_name); \
    lua_setfield (L, -2, #_name); \
    }
    CMD(name);
    CMD(open);
    CMD(sym2addr);
    CMD(addr2sym);
    CMD(sym2die);
    CMD(doodle);

    CMD(die_log);

    CMD(die_attr);

    CMD(make_DW_AT);

#undef CMD
    return 1;
}

// FIXME: Is there a sane way to set up paths?
// This handles require("lib.elfutils_lua51")
int luaopen_lib_elfutils_lua51 (lua_State *L) {
    return luaopen_elfutils_lua51 (L);
}
