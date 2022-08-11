
/* Based on typeswitch's C99 Forth DSL:
   https://twitter.com/typeswitch/status/1543749577647456258

   Mixed with ideas from Frank Sergeant's 3-instruction Forth
   https://pages.cs.wisc.edu/~bolo/shipyard/3ins4th.html
   which are also used in Staapl's Forth monitor

   The primary application is to replace gdbstub on target with a
   minimal Forth and a gdbstub or other protocol wrapper running as a
   Linux daemon.

   Implementation should go into a separate compilation unit to allow
   use of concise macros that would otherwise pollute the global
   namespace.

   Some extensions: this supports both RAM and ROM.

*/

#ifndef MOD_FORTH_DSL
#define MOD_FORTH_DSL

#include "forth_dsl.h"

#ifndef RAMLEN
#error need RAMLEN
#endif

#ifndef ROMLEN
#error need ROMLEN
#endif

/* Opcode name<->number mapping is needed for external compilation, so
   put the definition in a macro iterator. */
#define FOR_OP(m) \
    m(KEY) m(EMIT) m(EXEC) m(SWAP) m(DROP) m(LIT) m(ADD) \
    m(JUMP)

/* In this files the opcodes are C identfiers collecte in this enum. */
#define REC_INIT(word) word,
enum OP { FOR_OP(REC_INIT) };

enum LABEL {
    /* After primtiive code space we can place RAM words. */
    OUTER=RAMLEN+0x20,
};
typedef uint16_t CELL;
static const CELL rom_cells[ROMLEN] = {
    [OUTER-RAMLEN] = LIT, '1', KEY, ADD, EMIT, JUMP, OUTER,
};
static CELL ram_cells[RAMLEN] = {
};

#define IP   (ram_cells[0])
#define DS   (ram_cells[1])
#define RS   (ram_cells[2])

/* 64k words of virtual memory. */
static CELL *mem(CELL addr) {
    if (addr < RAMLEN) return &ram_cells[addr];
    return (CELL*)&rom_cells[addr-RAMLEN];
}

/* Use wrapped access for RAM? */
//#define REF(N) (ram_cells[(CELL)(N) % RAMLEN])
#define REF(N) (*mem(N))

#define DS0  (REF(DS+0))
#define DS1  (REF(DS+1))
#define RS0  (REF(RS+0))
#define RS1  (REF(RS+1))
#define IP0  (REF(IP))

/* Machine suspends on input (KEY), and output is buffered (EMIT). */
static inline void push_key(struct forth_dsl_env *s, uint8_t key) {
    enum OP OP;
    goto resume;
  next:
    OP=REF(IP++);
    LOG("IP=%04x, OP=%02x\n", IP-1, OP);
  execute:
    switch(OP) {
    case EXEC: OP = DS0; DS++;                     goto execute;
    case JUMP: IP = IP0;                           goto next;
    case KEY:  return; resume: DS--; DS0=key;      goto next;
    case EMIT: cbuf_put(&s->out, DS0); DS++;       goto next;
    case DROP: DS++;                               goto next;
    case LIT:  DS--; DS0 = IP0; IP++;              goto next;
    case SWAP: {CELL T = DS0; DS0 = DS1; DS1 = T;} goto next;
    case ADD:  DS1 += DS0; DS++;                   goto next;
    }
    RS--; RS0=IP; IP=OP;                           goto next;
}

void forth_dsl_write(struct forth_dsl_env *s, const uint8_t *buf, uint32_t len) {
    for (uint32_t i=0; i<len; i++) {
        push_key(s, buf[i]);
    }
}
void forth_dsl_init(struct forth_dsl_env *s) {
    CBUF_INIT(s->out);
    IP = OUTER;

    /* RS is placed after DS to absorb dummy_key push during boot. */
    DS = 0x10 + 1;
    RS = 0x02;

    /* Execute up to the first occurance of KEY.  The key we pass in
       here will be ignored. */
    uint8_t dummy_key = 0;
    forth_dsl_write(s, &dummy_key, 1);
}


#endif
