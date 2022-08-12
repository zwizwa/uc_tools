/* Threaded code for one of the forth_dsl interpreters. */
#include "forth_dsl.h"

#ifndef MOD_FORTH_DSL_TC
#define MOD_FORTH_DSL_TC

/* Opcode name<->number mapping is needed for external compilation, so
   put the definition in a macro iterator. */
#define FORTH_DSL_FOR_PRIM(m)               \
    m(KEY,0)  m(EMIT,1) m(EXEC,2) m(SWAP,3) \
    m(DROP,4) m(LIT,5)  m(ADD,6)  m(JUMP,7) \
    m(EXIT,8)

/* Threaded code defined in this file can go into ROM, which starts at
   address 0 in the virtual memory. */
#define FORTH_DSL_FOR_LABEL(m) \
    m(OUTER,0)       \
    m(ADD1,OUTER+5)

#define FORTH_DSL_PRIM_ENUM_INIT(word,N) word = 0xFFFF-(N),
enum PRIM { FORTH_DSL_FOR_PRIM(FORTH_DSL_PRIM_ENUM_INIT) };

#define FORTH_DSL_LABEL_ENUM_INIT(word,N) word = N,
enum LABEL { FORTH_DSL_FOR_LABEL(FORTH_DSL_LABEL_ENUM_INIT) };

static const CELL forth_dsl_rom[] = {
    [OUTER] = KEY, ADD1, EMIT, JUMP, OUTER,
    [ADD1]  = LIT, 1, ADD, EXIT,
};


#endif
