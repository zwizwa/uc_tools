#ifndef  OSC_H
#define  OSC_H

#include "macros.h"
#include <stdint.h>

/* Minimal OSC implementation. */

/* Basic osc structure:
   - str: hierarchical address
   - str: payload type tags
   - binary payload

   All strings are 0-padded to 4-byte boundary.
 */

// FIXME: Use safe string methods.

union osc {
    char    s[4];
    float   f;
    int32_t i;
};
CT_ASSERT(osc_size, sizeof(union osc) == sizeof(uintptr_t));

struct param;

struct param_context {
    const struct param * const* root;
};

/* Parameters are in a tree structure. */
typedef void (*osc_float)(struct param_context *, float);
typedef void (*osc_int)  (struct param_context *, int32_t);

/* All pointers in this struct point to const data since the struct is
   intended to go into Flash memory. */
struct param;
struct param {
    const char *name;
    uintptr_t type;
    union {
        // leaf value handlers
        const osc_float f;
        const osc_int i;
        // null-terminated array of param pointers
        const struct param *const *p;
    } cont;
};
#define OSC_TYPE_LIST  0
#define OSC_TYPE_FLOAT 1
#define OSC_TYPE_INT   2




static inline int osc_word(int n) {
    return ((n-1)/4)+1;
}

// Length and setters are separate so caller can alloc.
static inline uintptr_t osc_len_scalar(uintptr_t addr_len) {
    uintptr_t n_slots = osc_word(addr_len+1);
    return n_slots + 2;
}


static inline uintptr_t osc_make_scalar(union osc *msg,   uintptr_t msg_len,
                                        const char *addr, uintptr_t addr_len,
                                        char type) {
    // msg_len as allocated by osc_len_scalar
    uintptr_t i_type    = msg_len - 2;
    uintptr_t i_payload = msg_len - 1;
    memset((void*)msg, 0, msg_len * sizeof(msg[0]));
    memcpy((void*)msg, addr, addr_len);
    msg[i_type].s[0] = ',';
    msg[i_type].s[1] = type;
    return i_payload;
}
static inline void osc_make_float(union osc *msg,   uintptr_t msg_len,
                                  const char *addr, uintptr_t addr_len,
                                  float val) {
    uintptr_t i = osc_make_scalar(msg, msg_len, addr, addr_len, 'f');
    msg[i].f = val;
}
static inline void osc_make_int(union osc *msg,   uintptr_t msg_len,
                                const char *addr, uintptr_t addr_len,
                                int32_t val) {
    uintptr_t i = osc_make_scalar(msg, msg_len, addr, addr_len, 'i');
    msg[i].i = val;
}

static inline const struct param *osc_find(const struct param * const* pl, const char *name) {
    // LOG("find: %s\n", name);
    for(;;){
        const struct param *p = pl[0]; // There is always a NULL terminator.
        if (!p) return NULL;
        // LOG("- check %s\n", p->name);
        if (!strcmp(name, p->name)) return p;
        pl++;
    }
}

#define OSC_PARSE_OK           0
#define OSC_PARSE_MISSING      1
#define OSC_PARSE_WILDCARD     2
#define OSC_PARSE_NOT_FOUND    3
#define OSC_PARSE_EXTRA        4
#define OSC_PARSE_BAD_TYPE     5
#define OSC_PARSE_NOT_SCALAR   6
#define OSC_PARSE_EXPECT_INT   7
#define OSC_PARSE_EXPECT_FLOAT 8
#define OSC_PARSE_UNKNOWN_TYPE 9

static inline const char *osc_error(int e) {
    static const char *errors[] = {
        [ OSC_PARSE_OK ]           = "success",
        [ OSC_PARSE_MISSING ]      = "missing address component",
        [ OSC_PARSE_WILDCARD ]     = "wildcard not supported",
        [ OSC_PARSE_NOT_FOUND ]    = "parameter not found",
        [ OSC_PARSE_EXTRA ]        = "extra address component",
        [ OSC_PARSE_BAD_TYPE ]     = "bad type syntax",
        [ OSC_PARSE_NOT_SCALAR ]   = "only supporting scalars",
        [ OSC_PARSE_EXPECT_INT ]   = "expected integer",
        [ OSC_PARSE_EXPECT_FLOAT ] = "expected float",
        [ OSC_PARSE_UNKNOWN_TYPE ] = "unknown type tag",
    };
    if (e < 0) return "unknown";
    if (e > ARRAY_SIZE(errors)) return "unknown";
    return errors[e];
}

static inline int osc_parse(struct param_context *x, const union osc *cmd_ro, uintptr_t nb_cmd ) {
    const struct param * const* pl = x->root;
    // LOG("root0: %s\n", pl[0]->name);

    union osc cmd[nb_cmd];
    memcpy(cmd, cmd_ro, sizeof(cmd));
    // LOG("cmd: %s\n", cmd[0].s);
    int i_type = osc_word(strlen(cmd[0].s));
    const char delim[] = "/";
    char *tok;
    char *tok_param = cmd[0].s;

  next:
    if (!(tok = strtok(tok_param, delim))) {
        return OSC_PARSE_MISSING;
    }
    tok_param = NULL;
    // LOG("tok: %s\n", tok);
    if (tok[0] == '*') {
        return OSC_PARSE_WILDCARD;
    }
    const struct param *p = osc_find(pl, tok);
    if (!p) {
        return OSC_PARSE_NOT_FOUND;
    }
    // LOG("found %s type=%d\n", tok, p->type);
    if (p->type == OSC_TYPE_LIST) {
        /* Recurse tree */
        pl = p->cont.p;
        goto next;
    }
    else {
        /* Handle leaf.  Input address needs to be at the end */
        if ((tok = strtok(NULL, delim))) {
            return OSC_PARSE_EXTRA;
        }
        char *t = cmd[i_type].s;
        // LOG("type: %s\n", t);
        if (t[0] != ',') {
            return OSC_PARSE_BAD_TYPE;
        }
        if (t[2] != 0) {
            LOG("only supporting scalars\n");
            return OSC_PARSE_NOT_SCALAR;
        }
        union osc *w = &cmd[i_type+1];
        switch(p->type) {
        case OSC_TYPE_FLOAT:
            /* Expecting float */
            if (t[1] != 'f') {
                return OSC_PARSE_EXPECT_FLOAT;
            }
            // LOG("float: %f\n", w->f);
            p->cont.f(x, w->f);
            return OSC_PARSE_OK;
        case OSC_TYPE_INT:
            /* Expecting int */
            if (t[1] != 'i') {
                return OSC_PARSE_EXPECT_INT;
            }
            // LOG("int: %d\n", w->i);
            p->cont.i(x, w->i);
            return OSC_PARSE_OK;
        default:
            LOG("bad type %d\n", p->type);
            return OSC_PARSE_UNKNOWN_TYPE;
        }
    }
    /* Not reached */
}


#define DEF_OSC_FLOAT(_cname, _name, _fun)                              \
    const struct param _cname = {.name = _name, .type = OSC_TYPE_FLOAT, .cont = { .f = _fun }}
#define DEF_OSC_INT(_cname, _name, _fun)                                \
    const struct param _cname = {.name = _name, .type = OSC_TYPE_INT,   .cont = { .i = _fun }}

#define DEF_OSC_LIST(_cname, _name, ...)                                \
    const struct param *_cname##_list[] = {__VA_ARGS__ , NULL};               \
    const struct param _cname = {.name = _name, .type = OSC_TYPE_LIST, .cont = { .p = _cname##_list }};

#endif
