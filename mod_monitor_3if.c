/* Staapl style 3-Instruction-Forth after attempting mod_forth_dsl.c
   which is not exactly what I need.

   See:
   - Staapl project pic18/interpreter.f
   - https://pages.cs.wisc.edu/~bolo/shipyard/3ins4th.html

   Differences to Stappl interpreter:
   - Codes are different
   - JSR doesn't load code ptr, LDC loads code ptr
   - Uses uintptr_t register lengths for ram, flash, code registers

   The idea is that this can go on a microcontroller taking up very
   little code space.  The host side can then read/write to Flash and
   RAM and execute code.

*/

#ifndef MOD_MONITOR_3IF
#define MOD_MONITOR_3IF

#include "macros.h"
#include "cbuf.h"
#include <stdint.h>


/* Same encoding as Staapl.
   A = rAm pointer
   F = Flash pointer
   C = Code pointer
*/

/* Codes are mapped to 0x8x.  This moves it away from:
   - 7-bit clean ASCII
   - SLIP encoding
   - low byte count {packet,N} messages

   This ensures that protocol errors can be used to cause transparent
   protocol switches for things based on. */

#define MONITOR_3IF_FOR_PRIM(m)                      \
    m(ACK,  0)  m(NPUSH, 1)  m(NPOP, 2)  m(JSR,  3)  \
    m(LDA,  4)  m(LDF,   5)  m(LDC,  6)  m(INTR, 7)  \
    m(NAL,  8)  m(NFL,   9)  m(NAS, 11)  m(NFS, 12)  \

#define PRIM_ENUM_INIT(word,N) word = (0x80 + N),
enum PRIM { MONITOR_3IF_FOR_PRIM(PRIM_ENUM_INIT) };

/* Machine is written in push style, waiting for next byte.  There are
   multiple suspend points. */
#define NEXT_LABEL(s,var,label)                         \
    do {						\
	s->next = &&label;				\
	return 0;                                       \
      label: 						\
        (var) = key;                                    \
    } while(0)
#define NEXT(s,var)                             \
    NEXT_LABEL(s,var,GENSYM(label_))

struct monitor_3if;
struct monitor_3if {
    /* State used by Forth primitives. */
    /* 0 */ uint8_t *ds;
    /* 1 */ uint8_t *rs;

    /* State used by host */
    /* 2 */ uint8_t *ram;
    /* 3 */ uint8_t *flash;
    /* 4 */ void (*code)(struct monitor_3if *s);

    /* Internal interpreter state */
    /* 5 */ void *next;  // continuation after byte input
    /* 6 */ void *cont;  // continuation after end of loop
    /* 7 */ void (*transfer)(struct monitor_3if *s);  // what to do inside loop
    /* 8 */ struct cbuf *out; // output buffer
    /* 9 */ uint16_t count; uint8_t offset; uint8_t byte;

};

#define REG(name) OFFSETOF(struct monitor_3if, name)


#define DS (s->ds)
#define DS0 (DS[0])

/* Machine suspends on input (KEY), and output is buffered (EMIT). */

/* FIXME: Machine should suspend on output as well.  For now we assume
   the out buffer is always large enough, essentially moving flow
   control to the host end. */

/* Loop body loaded into s->code. */
void from_ram  (struct monitor_3if *s) { s->byte = *(s->ram)++; }
void from_flash(struct monitor_3if *s) { s->byte = *(s->flash)++; }

void to_ram    (struct monitor_3if *s) { *(s->ram)++ = s->byte; }
void to_flash  (struct monitor_3if *s) { /* FIXME */; }

void to_state  (struct monitor_3if *s) { ((uint8_t*)s)[s->offset++] = s->byte; }

/* The underlying Forth VM is fundamentally an 8-bit machine.  The
   orientation of the stack is such that multi-byte data has native
   byte order in the data coming FROM the host, and in the data stored
   in the stack memory.  This means that reading out TO the host will
   invert the data.  Note that it is possible to use from_ram with ram
   pointer pointing into stack to get proper byte order readout. */
void push_stack(struct monitor_3if *s) { *(s->ds)++ = s->byte; }
void pop_stack (struct monitor_3if *s) { s->byte = *--(s->ds); }

static inline uintptr_t push_key(struct monitor_3if *s, uint8_t key) {
    enum PRIM op;
    if (s->next) goto *(s->next);
  next:
    NEXT(s,op);
    LOG("op=%02x\n", op);
    switch(op) {

    case ACK:                              goto ack;

        /* LOAD */

    case NAL:   s->transfer = from_ram;    goto loop_from;
    case NFL:   s->transfer = from_flash;  goto loop_from;
    case NPOP:  s->transfer = pop_stack;   goto loop_from;

        /* STORE */

    case NAS:   s->transfer = to_ram;      goto loop_to;
    case NFS:   s->transfer = to_flash;    goto loop_to;
    case NPUSH: s->transfer = push_stack;  goto loop_to;

    case LDA:   s->offset = REG(ram);      goto to_reg;
    case LDF:   s->offset = REG(flash);    goto to_reg;
    case LDC:   s->offset = REG(code);     goto to_reg;

        /* EXECUTE */

    case JSR:   s->code(s);                goto ack;
    case INTR:  s->code(s);                goto next;

    }

    /* Protocol error. */
    return 1;

  loop_from:
    // needs s->transfer
    NEXT(s, s->count);
    cbuf_put(s->out, s->count);
    s->cont = &&next;
    while(s->count--) { s->transfer(s); cbuf_put(s->out, s->byte);}
    goto *(s->cont);

  to_reg:
    // needs s->offset
    s->transfer = to_state;
    s->count = sizeof(uintptr_t);
    goto loop_to_inner;
  loop_to:
    // needs s->transfer
    NEXT(s, s->count);
  loop_to_inner:
    while(s->count--) { NEXT(s, s->byte); s->transfer(s); }
    goto ack;

  ack:
    cbuf_put(s->out, 0); // 0-size reply
    goto next;
}

uintptr_t monitor_3if_write(struct monitor_3if *s, const uint8_t *buf, uint32_t len) {
    for (uint32_t i=0; i<len; i++) {
        uintptr_t status = push_key(s, buf[i]);
        if (status) return status;
    }
    return 0;
}
void monitor_3if_init(struct monitor_3if *s, struct cbuf *out, uint8_t *ds_buf, uint8_t *rs_buf) {
    memset(s,0,sizeof(*s));
    cbuf_clear(out);
    s->out = out;
    s->ds = ds_buf;
    s->rs = rs_buf;
    /* Dummy write to end up in the first blocking read. */
    uint8_t dummy = 0; monitor_3if_write(s, &dummy, 1);
}


#endif
