
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

#define FORTH_DSL_BITS
#include "forth_dsl.h"


/* Stack pointers are not accessible from the OPs, so representation
   can just as well be direct offsets into ram[], not using the mem()
   indirection. */
#define DS0 (DS[0])
#define DS1 (DS[1])
#define RS0 (RS[0])

/* Machine suspends on input (KEY), and output is buffered (EMIT). */
static inline void push_key(struct forth_dsl_state *s, uint8_t key) {
    enum OP OP;
    CELL *DS = s->ds;
    CELL *RS = s->rs;
    CELL IP  = s->ip;
    goto resume;
  next:
    OP=MEM(IP++);
    LOG("IP=%04x OP=%04x DS(0)=%04x\n", IP-1, OP, DS0);
  execute:
    switch(OP) {
    case EXEC: OP=DS0; DS++;                      goto execute;
    case JUMP: IP=MEM(IP);                        goto next;
    case EXIT: IP=RS0; RS++;                      goto next;
    case KEY:  goto yield; resume: DS--; DS0=key; goto next;
    case EMIT: cbuf_put(&s->out, DS0); DS++;      goto next;
    case DROP: DS++;                              goto next;
    case LIT:  DS--; DS0=MEM(IP); IP++;           goto next;
    case SWAP: {CELL T=DS0; DS0=DS1; DS1=T;}      goto next;
    case ADD:  DS1+=DS0; DS++;                    goto next;
    }
    RS--; RS0=IP; IP=OP;                          goto next;
  yield:
    s->ds = DS;
    s->rs = RS;
    s->ip = IP;
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
