
/* Based on typeswitch's C99 Forth DSL:
   https://twitter.com/typeswitch/status/1543749577647456258

   Mixed with ideas from Frank Sergeant's 3-instruction Forth
   https://pages.cs.wisc.edu/~bolo/shipyard/3ins4th.html
   which are also used in Staapl's Forth monitor

   Remarks:

   - Context this was written in: replace uc_tools STM32F103
     bootloader's gdbstub on target with a minimal Forth, then write a
     gdbstub or other protocol wrapper running as a Linux daemon to
     restore old functionality.

   - Implementation should go into a separate compilation unit to
     allow use of concise names that would otherwise pollute the
     global namespace.

   - Memory model is "ROM first", such that typeswitch's C initializer
     can be used to encode threaded code in the C file.  Then RAM can
     be used for compilation of new run time words.  The primitives
     are mapped at the end of the 16-bit memory space.

*/

#ifndef MOD_FORTH_DSL
#define MOD_FORTH_DSL

#include "forth_dsl.h"

#ifndef RAMLEN
#error need RAMLEN
#endif

typedef uint16_t CELL;

/* Opcode name<->number mapping is needed for external compilation, so
   put the definition in a macro iterator. */
#define FOR_OP(m)                           \
    m(KEY,0)  m(EMIT,1) m(EXEC,2) m(SWAP,3) \
    m(DROP,4) m(LIT,5)  m(ADD,6)  m(JUMP,7) \
    m(EXIT,8)

/* Expose opcodes as enum. */
#define OP_ENUM_INIT(word,N) word = 0xFFFF-(N),
enum OP { FOR_OP(OP_ENUM_INIT) };

/* Threaded code defined in this file can go into ROM, which starts at
   address 0 in the virtual memory. */
#define FOR_LABEL(m) \
    m(OUTER,0)       \
    m(ADD1,OUTER+5)

#define LABEL_ENUM_INIT(word,N) word = N,
enum LABEL { FOR_LABEL(LABEL_ENUM_INIT) };

static const CELL rom[] = {
    [OUTER] = KEY, ADD1, EMIT, JUMP, OUTER,
    [ADD1]  = LIT, 1, ADD, EXIT,
};
#define ROMLEN ARRAY_SIZE(rom)

/* RAM is not initialized. */
static CELL ram[RAMLEN];
#define IP (ram[0])
#define DS (ram[1])
#define RS (ram[2])

/* 64k words of virtual memory. */
static __attribute__((__const__)) CELL *mem(CELL addr)  {
    if (addr < ROMLEN) return (CELL*)&rom[addr];
    addr -= ROMLEN;
    if (addr < RAMLEN) return &ram[addr];
    return 0;
}
#define MEM(N) (*mem(N))
#define IP0 (MEM(IP))
// FIXME: These are always RAM, so de-virtualize the DS/RS pointers?
#define DS0 (MEM(DS+0))
#define DS1 (MEM(DS+1))
#define RS0 (MEM(RS+0))
#define RS1 (MEM(RS+1))

#define RAM_CELL(N) (ROMLEN + (N))

/* Machine suspends on input (KEY), and output is buffered (EMIT). */
static inline void push_key(struct forth_dsl_env *s, uint8_t key) {
    enum OP OP;
    goto resume;
  next:
    OP=MEM(IP++);
    LOG("IP=%04x OP=%04x DS0=%04x\n", IP-1, OP, DS0);
  execute:
    switch(OP) {
    case EXEC: OP=DS0; DS++;                  goto execute;
    case JUMP: IP=IP0;                        goto next;
    case EXIT: IP=RS0; RS++;                  goto next;
    case KEY:  return; resume: DS--; DS0=key; goto next;
    case EMIT: cbuf_put(&s->out, DS0); DS++;  goto next;
    case DROP: DS++;                          goto next;
    case LIT:  DS--; DS0=IP0; IP++;           goto next;
    case SWAP: {CELL T=DS0; DS0=DS1; DS1=T;}  goto next;
    case ADD:  DS1+=DS0; DS++;                goto next;
    }
    RS--; RS0=IP; IP=OP;                      goto next;
}

void forth_dsl_write(struct forth_dsl_env *s, const uint8_t *buf, uint32_t len) {
    for (uint32_t i=0; i<len; i++) {
        push_key(s, buf[i]);
    }
}
void forth_dsl_init(struct forth_dsl_env *s) {
    CBUF_INIT(s->out);
    IP = OUTER;
    DS = RAM_CELL(0x10 + /* dummy_key compensation */ 1);
    RS = RAM_CELL(0x20);
    /* Execute up to the first occurrence of KEY so that the machine
       is waiting for data.  The key we pass in here will be ignored
       and we just compensate for the DS push on first resume. */
    uint8_t dummy_key = 0;
    forth_dsl_write(s, &dummy_key, 1);
}


#endif
