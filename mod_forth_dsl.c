
/* Based on typeswitch's C99 Forth DSL:
   https://twitter.com/typeswitch/status/1543749577647456258

   Mixed with ideas from Frank Sergeant's 3-instruction Forth
   https://pages.cs.wisc.edu/~bolo/shipyard/3ins4th.html
   which are also used in Staapl's Forth monitor

   The primary application is to replace gdbstub on target and replace
   it with a minimal Forth on target and a gdbstub or other protocol
   wrapper running as a Linux daemon.

   Implementation should go into a separate compilation unit to allow
   use of concise macros that would otherwise pollute the global
   namespace.

*/

#ifndef MOD_FORTH_DSL
#define MOD_FORTH_DSL

#include "forth_dsl.h"

#ifndef RAMLEN
#error need RAMLEN
#endif

/* Opcodes are needed for external compilation, so put them in a macro
   iterator. */
#define FOR_OP(m) \
    m(KEY) m(EMIT) m(EXEC) m(SWAP) m(DROP) m(LIT)

/* In this files the opcodes are C identfiers collecte in this enum. */
#define REC_INIT(word) word,
static enum OP { FOR_OP(REC_INIT) } OP;

enum LABEL {
    RDS=8,
    RRS=9,
    RIP=10,
    RHERE=11,
    BOOT=0x10,
    OUTER=0x10,
    INITIAL_HERE=0x100,
};
typedef uint16_t CELL;
static CELL RAM[RAMLEN] = {
    /* registers */
    [RDS] = RAMLEN - 0x20,
    [RRS] = RAMLEN - 0x10,
    [RIP] = BOOT,
    [RHERE] = INITIAL_HERE,
    [INITIAL_HERE] = 0,

    /* threaded code */
    [OUTER] = LIT, '!', EMIT, KEY, DROP, OUTER
};

#define IP   (RAM[RIP])
#define DS   (RAM[RDS])
#define RS   (RAM[RRS])
#define HERE (RAM[RHERE])

/* Use wrapped access for RAM. */
#define WRAM(N) (RAM[(CELL)(N) % RAMLEN])

#define DS0  (WRAM(DS+0))
#define DS1  (WRAM(DS+1))
#define RS0  (WRAM(RS+0))
#define RS1  (WRAM(RS+1))
#define IP0  (WRAM(IP))

/* Machine suspends on input (KEY), and output is buffered (EMIT). */
static inline void push_key(struct forth_dsl_env *s, uint8_t key) {
    CELL TEMP;
    goto resume;
    for(;;) {
        OP=WRAM(IP++);
        LOG("IP=%04x, OP=%02x\n", IP-1, OP);
      exec:
        switch(OP) {
        case KEY:  return; resume: DS--; DS0=key;      continue;
        case EMIT: cbuf_put(&s->out, DS0); DS++;       continue;
        case EXEC: OP = DS0; DS++;                     goto exec;
        case SWAP: TEMP = DS0; DS0 = DS1; DS1 = TEMP;  continue;
        case DROP: DS++;                               continue;
        case LIT:  DS--; DS0 = IP0; IP++;              continue;
        }
        RS--; RS0=IP; IP=OP;
    }
}

void forth_dsl_write(struct forth_dsl_env *s, const uint8_t *buf, uint32_t len) {
    for (uint32_t i=0; i<len; i++) {
        push_key(s, buf[i]);
    }
}
void forth_dsl_init(struct forth_dsl_env *s) {
    CBUF_INIT(s->out);
}


#endif
