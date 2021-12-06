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
// want that dependency.

// Libelf requires some knowledge of the structure of ELF files.
// https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

// Build against 5.1 for best compatibility with both Lua and LuaJIT

#if 0
// Debian, without -I/usr/local/lua5.1
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>
#else
#include "lua.h"
#include "lauxlib.h"
#endif

// elfutils
#include <libelf.h>
#include <elfutils/libdw.h>
#include <dwarf.h>

#include <stdarg.h>

// These might be useful for other things, so are in a separate file.
#include "elfutils_tags.h"


// uc_tools/ macros.h will use this, expanded from ASSERT.
// The 'L' variable needs to be in lexical scope.
// I don't like this, but currently no quick way to fix it in macros.h
// FIXME: Parameterize macros.h later to let error abort take an explicit context.
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

/* Note that we need to manage elf_shared separately, because it is an
   internal object that is referenced by elf_ud (explicitly), and
   die_ud (implicitly, but made explicit for RC). */

struct elf_shared  {
    int fd;
    Elf *elf;
    GElf_Ehdr ehdr;      // translated copy of the ELF header
    Elf_Scn *symtab_scn; // symbol section
    size_t strtab_ndx;
    Dwarf *dwarf;
    int rc; // FIXME: not yet implemented
};
struct elf_ud {
    struct elf_shared *shared;
};

struct die_ud {
    Dwarf_Die die;
    struct elf_shared *elf;
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

static struct die_ud *push_die(lua_State *L, struct elf_shared *elf_shared) {
    ASSERT(elf_shared);
    struct die_ud *ud = lua_newuserdata(L, sizeof(*ud));
    ASSERT(ud);
    memset(ud,0,sizeof(*ud));
    ud->elf = elf_shared;
    luaL_getmetatable(L, T_DIE);
    lua_setmetatable(L, -2);
    elf_shared->rc++;
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

    struct elf_ud *ud = lua_newuserdata(L, sizeof(*ud));
    ASSERT(ud);
    memset(ud,0,sizeof(*ud));
    ud->shared = calloc(sizeof(*ud->shared),1);
    ASSERT(ud->shared);
    ud->shared->rc = 1;
    ud->shared->elf = elf;
    ud->shared->fd = fd;

    // Get translated copy of the ELF file header
    ASSERT(gelf_getehdr(elf, &ud->shared->ehdr));
    ASSERT(ud->shared->ehdr.e_shstrndx);

    size_t ndx = 0;
    Elf_Scn *scn;
    while((scn = elf_getscn(elf, ndx))) {

        GElf_Shdr shdr = {};
        gelf_getshdr(scn, &shdr);

        /* Section names are in the string section with index
           e_shstrndx linked from the ELF header. */
        const char *name =
            elf_strptr(ud->shared->elf,
                       ud->shared->ehdr.e_shstrndx,
                       shdr.sh_name);
        ASSERT(name);

        // LOG("%2d %s (0x%x)\n", i, name, shdr.sh_type);

        /* Cache some section information as index or pointer,
           depending on how it is used later. */
        if      (!strcmp(".strtab", name)) { ud->shared->strtab_ndx = ndx; }
        else if (!strcmp(".symtab", name)) { ud->shared->symtab_scn = scn; }

        ndx++;
    }
    ASSERT(ud->shared->symtab_scn);
    ASSERT(ud->shared->strtab_ndx);

    // Initialize DWARF index
    ASSERT(ud->shared->dwarf = dwarf_begin_elf(elf, DWARF_C_READ, NULL));

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
    ASSERT(i->ud);
    i->name = NULL;
    if (gelf_getsym(i->data_sym, i->ndx_sym, &i->sym) == NULL) return;
    i->name = elf_strptr(i->ud->shared->elf,
                         i->ud->shared->strtab_ndx,
                         i->sym.st_name);
    ASSERT(i->name);
    i->ndx_sym++;
}
static inline sym_iter_t sym_iter_new(struct elf_ud *ud, lua_State *L) {
    sym_iter_t i = { .ud = ud, .L = L };
    ASSERT(i.data_sym = elf_getdata(ud->shared->symtab_scn, 0));
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
    int nb_retvals;
    struct elf_shared *elf_shared;
    /* search filters. */
    const char *search_name;
    unsigned int search_tag;
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
        if (s->search_name
            && (!strcmp(name, s->search_name))
            && (s->search_tag == tag)) {

            struct die_ud *ud = push_die(s->L, s->elf_shared);
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
// FIXME: This can probably be moved to Lua
static int die_walk(die_walk_t *s, struct elf_ud *ud) {
    lua_State *L = s->L;
    Dwarf_Off off=0, next_off=0, abbrev_offset=0;
    size_t header_size=0;
    uint8_t address_size=0, offset_size=0;

    while(dwarf_nextcu(ud->shared->dwarf, off,
                       &next_off, &header_size, &abbrev_offset,
                       &address_size, &offset_size) == 0) {
        ASSERT(dwarf_offdie(ud->shared->dwarf, off + header_size, &s->die));
        walk_die_tree(s);
        off = next_off;
    }
    return 0;
}

/* To provide CU access to Lua it seems simplest to dump it as a list.
   This avoids a userdata object to represent the iterator state. */
static int cmd_compilation_units(lua_State *L) {
    struct elf_ud *elf_ud = L_elf(L, -1);
    Dwarf_Off off=0, next_off=0, abbrev_offset=0;
    size_t header_size=0;
    uint8_t address_size=0, offset_size=0;
    lua_newtable(L);
    Dwarf_Die die;
    int index = 1;
    while(dwarf_nextcu(elf_ud->shared->dwarf, off,
                       &next_off, &header_size, &abbrev_offset,
                       &address_size, &offset_size) == 0) {
        ASSERT(dwarf_offdie(elf_ud->shared->dwarf, off + header_size, &die));
        lua_pushnumber(L, index++);
        struct die_ud *die_ud = push_die(L, elf_ud->shared);
        die_ud->die = die;
        lua_settable(L, -3);
        off = next_off;
    }
    return 1;
}


/* leb128 decode of Dwarf_Block */
static uint32_t block_u32(Dwarf_Block *b) {
    // FIXME: THIS IS WRONG.
    // FIXME: Are the LEB128 functions from libdw accessible?
    // Currently just a special case.
    // ASSERT(b->length == 5);
    // ASSERT(b->data[0] == 3); // is actually LEB128
    // FIXME: assuming same endianness!
    return *((uint32_t*)(b->data+1));
}

static int find_die_type(lua_State *L, unsigned int dw_tag) {
    struct elf_ud *ud = L_elf(L, -2);
    const char *name = L_string(L, -1);
    die_walk_t s = {
        .L = L,  // for return data and errors
        .elf_shared = ud->shared,  // used by die_ud constructor
        .search_name = name, // if defined, return only this DIE
        .search_tag = dw_tag
    };
    die_walk(&s, ud);
    return s.nb_retvals;
}
static int cmd_die_find_variable(lua_State *L) {
    return find_die_type(L, DW_TAG_variable);
}
static int cmd_die_find_structure_type(lua_State *L) {
    return find_die_type(L, DW_TAG_variable);
}



struct die_attrs_ctx {
    lua_State *L;
    lua_Integer index;
};
static int push_attr_code(Dwarf_Attribute *attr, void *vctx) {
    struct die_attrs_ctx *ctx = vctx;
    lua_pushnumber(ctx->L, ctx->index++);
    lua_pushnumber(ctx->L, attr->code);
    lua_settable(ctx->L, -3);
    return DWARF_CB_OK;
}
static int cmd_die_attr_list(lua_State *L) {
    struct die_ud *die_ud = L_die(L, -1);
    struct die_attrs_ctx ctx = {.L = L, .index = 1};
    lua_newtable(L);
    dwarf_getattrs(&die_ud->die, push_attr_code, &ctx, 0);
    return 1;
}

static int cmd_die_cuoffset(lua_State *L) {
    struct die_ud *die_ud = L_die(L, -1);
    lua_pushnumber(L, dwarf_cuoffset(&die_ud->die));
    return 1;
}
static int cmd_die_tag(lua_State *L) {
    struct die_ud *die_ud = L_die(L, -1);
    lua_pushnumber(L, dwarf_tag(&die_ud->die));
    return 1;
}
// The tree is linked up as a single child link per die, where each
// child has a sibling link. Don't create the userdata until we know
// that we have something.
static int cmd_die_child(lua_State *L) {
    struct die_ud *in = L_die(L, -1);
    Dwarf_Die child;
    if (0 == dwarf_child(&in->die, &child)) {
        struct die_ud *out = push_die(L, in->elf);
        out->die = child;
        return 1;
    }
    return 0;
}
static int cmd_die_sibling(lua_State *L) {
    struct die_ud *in = L_die(L, -1);
    Dwarf_Die sibling;
    if (0 == dwarf_siblingof(&in->die, &sibling)) {
        struct die_ud *out = push_die(L, in->elf);
        out->die = sibling;
        return 1;
    }
    return 0;
}


/* Convert die attribute to something usable from Lua.
   To add support for other DW_FORM_ types do the following:

   1. Use the symbolic names for the DW_AT_ and DW_FORM_ codes in
      dwarf.h

   2. Look up the DW_AT_ code in DWARF4.pdf - that will give you an
      idea of how the value is encoded.

   3. Grep the libdw code for the specific DW_FORM_ name that needs to
      be decoded to see which dwarf_form* function should be
      called. */

int cmd_die_attr(lua_State *L) {
    struct die_ud *die_ud = L_die(L, -2);
    unsigned int code     = L_number(L, -1);  // DW_AT_* codes
    Dwarf_Attribute attr;
    ASSERT(dwarf_attr(&die_ud->die, code, &attr));
    switch(attr.form) {
    case DW_FORM_string:
    case DW_FORM_strp:
        lua_pushstring(L, dwarf_formstring(&attr));
        break;
    case DW_FORM_ref4: {
        struct die_ud *ud = push_die(L, die_ud->elf);
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
    /* FIXME: For these the sdata/udata distinction needs to be
       clarified from context.  We do not have a way to do that.  The
       sane default seems to be to interpret as udata.  If not, struct
       member offsets and sizes get read as sdata which is wrong. */
    case DW_FORM_data1:
    case DW_FORM_data2:
    case DW_FORM_data4: {
        Dwarf_Word uval;
        ASSERT(0 == dwarf_formudata(&attr, &uval));
        lua_pushnumber(L, uval);
        break;
    }
    case DW_FORM_flag_present: {
        bool flag;
        ASSERT(0 == dwarf_formflag(&attr, &flag));
        lua_pushnumber(L, flag);
        break;
    }
    case DW_FORM_sec_offset: {
        Dwarf_Word uval;
        ASSERT(0 == dwarf_formudata(&attr, &uval));
        lua_pushnumber(L, uval);
        break;
    }
    case DW_FORM_addr: {
        Dwarf_Addr addr;
        ASSERT(0 == dwarf_formaddr(&attr, &addr));
        lua_pushnumber(L, addr);
        break;
    }
    case DW_FORM_sdata: {
        Dwarf_Sword sval;
        ASSERT(0 == dwarf_formsdata(&attr, &sval));
        lua_pushnumber(L, sval);
        break;
    }
    default:
        ERROR("die_attr code=0x%x unsupported form=0x%x", attr.code, attr.form);
    }
    return 1;
}


// Reify the iteration macros as a Lua data structure.  To keep it
// simple, expand them as a C array first.
struct number_table {
    const char *name;
    const unsigned int code;
};
#define DW_AT_FIELD(name)  { #name, DW_AT_##name },
#define DW_TAG_FIELD(name) { #name, DW_TAG_##name },
const struct number_table DW_AT_table[]  = { FOR_DW_AT(DW_AT_FIELD) };
const struct number_table DW_TAG_table[] = { FOR_DW_TAG(DW_TAG_FIELD) };

// Then export those as a Lua tables.
static int cmd_get_DW_AT(lua_State *L) {
    lua_newtable(L);
    for(int i=0; i<ARRAY_SIZE(DW_AT_table); i++) {
        lua_pushnumber(L, DW_AT_table[i].code);
        lua_setfield(L, -2, DW_AT_table[i].name);
    }
    return 1;
}
static int cmd_get_DW_TAG(lua_State *L) {
    lua_newtable(L);
    for(int i=0; i<ARRAY_SIZE(DW_TAG_table); i++) {
        lua_pushnumber(L, DW_TAG_table[i].code);
        lua_setfield(L, -2, DW_TAG_table[i].name);
    }
    return 1;
}


static int cmd_doodle(lua_State *L) {
    struct elf_ud *ud = L_elf(L, -1);
    die_walk_t s = { .L = L, .verbose = 1 };
    return die_walk(&s, ud);
}

static void deref_die_shared(struct elf_shared *s, const char *typ) {
    s->rc--;
    if (!s->rc) {
        /* Object can be removed. */
        dwarf_end(s->dwarf);
        elf_end(s->elf);
        close(s->fd);
        free(s);
    }
}

static int gc_elf(lua_State *L) {
    struct elf_ud *ud = L_elf(L, -1);
    //LOG("gc_elf %p\n", ud);
    deref_die_shared(ud->shared, "elf");
    return 0;
}
static int gc_die(lua_State *L) {
    struct die_ud *ud = L_die(L, -1);
    //LOG("gc_die %p\n", ud);
    deref_die_shared(ud->elf, "die");
    return 0;
}

static void new_metatable(lua_State *L, const char *t_name, int (*gc)(lua_State *)) {
    // FIXME: Add __gc method.
    luaL_newmetatable(L, t_name);

    // FIXME: I want prompt to print the type name. How to do this
    // properly?  __name is not enough.
    if (1) {
        luaL_getmetatable(L, t_name);
        lua_pushstring(L, t_name); lua_setfield(L, -2, "__name");
        lua_pushcfunction(L, gc);  lua_setfield(L, -2, "__gc");
        lua_pop(L, -1);
    }

}

static int cmd_get_metatables(lua_State *L) {
    lua_newtable(L);
    /* The T_ are global names that can later be changed if necessary.
       We also return a table that is stored lexically which the lua
       code uses to type-dispatch. */
    luaL_getmetatable(L, T_ELF); lua_setfield(L, -2, "elf");
    luaL_getmetatable(L, T_DIE); lua_setfield(L, -2, "die");
    return 1;
}

int luaopen_elfutils_lua51 (lua_State *L) {

    new_metatable(L, T_ELF, gc_elf);
    new_metatable(L, T_DIE, gc_die);

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

    CMD(compilation_units);

    CMD(die_find_variable);
    CMD(die_find_structure_type);

    CMD(die_child);
    CMD(die_sibling);
    CMD(die_attr);
    CMD(die_attr_list);
    CMD(die_cuoffset);
    CMD(die_tag);

    CMD(get_DW_AT);
    CMD(get_DW_TAG);
    CMD(get_metatables);


#undef CMD
    return 1;
}

// FIXME: Is there a sane way to set up paths?
// This handles require("lib.elfutils_lua51")
int luaopen_lib_elfutils_lua51 (lua_State *L) {
    return luaopen_elfutils_lua51 (L);
}
