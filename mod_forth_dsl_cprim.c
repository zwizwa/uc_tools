/* A mod_forth_dsl.c variant using C function primitives.  This is a
   bit less space-efficient, but mixes better with C code. */

#ifndef MOD_FORTH_DSL
#define MOD_FORTH_DSL

#define FORTH_DSL_BITS
#include "forth_dsl.h"


/* Stack pointers are not accessible from the OPs, so representation
   can just as well be direct offsets into ram[], not using the mem()
   indirection. */
#define IP (s->ip)
#define DS (s->ds)
#define RS (s->rs)
#define DS0 (DS[0])
#define DS1 (DS[1])
#define RS0 (RS[0])

typedef void (*prim_t)(struct forth_dsl_state *s);

typedef struct forth_dsl_state *S;

/* Irregular control is implemented separately */
#define _EXEC 0
#define _KEY  0

void _EMIT(S s) { cbuf_put(&s->out, DS0); DS++; }
void _SWAP(S s) { CELL T=DS0; DS0=DS1; DS1=T; }
void _DROP(S s) { DS++; }
void _LIT (S s) { DS--; DS0=MEM(IP); IP++; }
void _ADD (S s) { DS1+=DS0; DS++; }
void _JUMP(S s) { IP = MEM(IP); }
void _EXIT(S s) { IP=RS0; RS++; }

#define OP_TABLE_INIT(word,N) [N] = _##word,
static const prim_t prim[] = { FOR_OP(OP_TABLE_INIT) };
static inline int is_prim(enum OP op) {
    return op >= (0x10000 - ARRAY_SIZE(prim));
}
#define DO_PRIM(op,s) ((prim[0xFFFF - op])(s))

/* Machine suspends on input (KEY), and output is buffered (EMIT). */
static inline void push_key(struct forth_dsl_state *s, uint8_t key) {
    enum OP op;
    /* resume KEY */
    DS--; DS0 = key;
  next:
    op=MEM(IP++);
  execute:
    LOG("IP=%04x op=%04x DS(0)=%04x\n", IP-1, op, DS0);
    switch(op) {
    case KEY:  return;
    case EXEC: op = DS0; DS++; goto execute;
    default:
        if (is_prim(op)) { DO_PRIM(op,s); }
        else             { RS--; RS0 = IP; IP = op; }
        goto next;
    }
}

void forth_dsl_write(struct forth_dsl_state *s, const uint8_t *buf, uint32_t len) {
    for (uint32_t i=0; i<len; i++) {
        push_key(s, buf[i]);
    }
}
void forth_dsl_init(struct forth_dsl_state *s) {
    CBUF_INIT(s->out);
    s->rom = forth_dsl_rom;
    s->ip = OUTER;
    /* DS, RS directly point into RAM array */
    s->ds = s->ram + 0x10 + /* dummy_key compensation */ 1;
    s->rs = s->ram + 0x20;
    /* Execute up to the first occurrence of KEY so that the machine
       is waiting for data.  The key we pass in here is an artefact of
       the implementation and will be ignored. */
    uint8_t dummy_key = 0;
    forth_dsl_write(s, &dummy_key, 1);
}


#endif
