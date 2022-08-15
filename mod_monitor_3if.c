/* Back to basics: doing an actual Staapl style 3IF after attempting
   mod_forth_dsl.c which is not exactly what I need.

   See Staapl project pic18/interpreter.f

   Differences:
   - Pointers are 32 bit

*/

#ifndef MOD_MONITOR_3IF
#define MOD_MONITOR_3IF

#include "macros.h"
#include "cbuf.h"
#include <stdint.h>


/* Same encoding as Staapl.
   A = rAm pointer
   F = Flash pointer */

#define MONITOR_3IF_FOR_PRIM(m)                    \
    m(ACK,0)  m(NPUSH, 1)  m(NPOP, 2)  m(JSR,  3)  \
    m(LDA,4)  m(LDF,   5)              m(INTR, 7)  \
    m(NAL,8)  m(NFL,   9)  m(NAS, 10)  m(NFS, 11)

#define PRIM_ENUM_INIT(word,N) word = N,
enum PRIM { MONITOR_3IF_FOR_PRIM(PRIM_ENUM_INIT) };

/* Machine is written in push style, waiting for next byte.  There are
   multiple suspend points. */
#define NEXT_LABEL(s,var,label)                         \
    do {						\
	s->next = &&label;				\
	return;						\
      label: 						\
        (var) = key;                                    \
    } while(0)
#define NEXT(s,var)                             \
    NEXT_LABEL(s,var,GENSYM(label_))

struct monitor_3if;
struct monitor_3if {
    /* Forth */
    /* 0 */ uint8_t *ds;

    /* Interpreter */
    /* 1 */ void *next;
    /* 2 */ uint8_t *ram;
    /* 3 */ uint8_t *flash;
    /* 4 */ void (*code)(void);
    /* 5 */ void *cont;
    /* 6 */ void (*transfer)(struct monitor_3if *s);
    uint16_t count;
    uint8_t offset, tmp;
    struct cbuf out; uint8_t out_buf[256];
};

#define REG(name) OFFSETOF(struct monitor_3if, name)


#define DS (s->ds)
#define DS0 (DS[0])

/* Machine suspends on input (KEY), and output is buffered (EMIT). */

/* FIXME: Machine should suspend on output as well.  For now we assume
   the out buffer is always large enough, essentially moving flow
   control to the host end. */

/* Loop body loaded into s->code. */
void from_ram  (struct monitor_3if *s) { s->tmp = *(s->ram)++; }
void from_flash(struct monitor_3if *s) { s->tmp = *(s->flash)++; }

void to_ram    (struct monitor_3if *s) { *(s->ram)++ = s->tmp; }
void to_flash  (struct monitor_3if *s) { /* FIXME */; }

void to_state  (struct monitor_3if *s) { ((uint8_t*)s)[s->offset++] = s->tmp; }

/* FIXME: Check byte order. */
void from_stack(struct monitor_3if *s) { s->tmp = *(s->ds)++; }
void to_stack  (struct monitor_3if *s) { *--(s->ds) = s->tmp; }

static inline void push_key(struct monitor_3if *s, uint8_t key) {
    /* Contains any op, but we set type as enum PRIM such that
       compiler warns if case statement is not total. */
    enum PRIM op;
    if (s->next) goto *(s->next);
  next:
    NEXT(s,op);
    switch(op) {

    case ACK:                              goto ack;

    case JSR:   s->offset = REG(code);     goto to_reg;
    case LDA:   s->offset = REG(ram);      goto to_reg;
    case LDF:   s->offset = REG(flash);    goto to_reg;

    case NAL:   s->transfer = from_ram;    goto loop_from;
    case NFL:   s->transfer = from_flash;  goto loop_from;
    case NPOP:  s->transfer = from_stack;  goto loop_from;

    case NAS:   s->transfer = to_ram;      goto loop_to;
    case NFS:   s->transfer = to_flash;    goto loop_to;
    case NPUSH: s->transfer = to_stack;    goto loop_to;

    case INTR: ASSERT(0); // FIXME: not used

    }

  loop_from:
    // needs s->transfer
    NEXT(s, s->count);
    cbuf_put(&s->out, s->count);
    s->cont = &&next;
    while(s->count--) { s->transfer(s); cbuf_put(&s->out, s->tmp);}
    goto *(s->cont);

  to_reg:
    // needs s->offset
    s->transfer = to_state;
    s->count = 4;
    goto loop_to_inner;
  loop_to:
    // needs s->transfer
    NEXT(s, s->count);
  loop_to_inner:
    s->cont = &&ack;
    while(s->count--) { NEXT(s, s->tmp); s->transfer(s); }
    goto *(s->cont);

  ack:
    cbuf_put(&s->out, 0); // 0-size reply
    goto next;
}

void monitor_3if_write(struct monitor_3if *s, const uint8_t *buf, uint32_t len) {
    for (uint32_t i=0; i<len; i++) {
        push_key(s, buf[i]);
    }
}
void monitor_3if_init(struct monitor_3if *s) {
    CBUF_INIT(s->out);
}


#endif
