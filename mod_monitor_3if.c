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

enum {
    A=1,F=2,C=3
} reg_nb;
struct monitor_3if {
    /* 0 */ void *next;
    /* 1 */ uint8_t *a;
    /* 2 */ uint8_t *f;
    /* 3 */ void (*c)(void);
    /* 4 */ void *cont;
    /* 5 */ uint8_t *ds;
    uint8_t count,reg,tmp,tmp2;
    struct cbuf out; uint8_t out_buf[256];
};

#define DS (s->ds)
#define DS0 (DS[0])

/* Machine suspends on input (KEY), and output is buffered (EMIT). */

/* FIXME: Machine should suspend on output as well.  For now we assume
   the out buffer is always large enough, essentially moving flow
   control to the host end. */



static inline void push_key(struct monitor_3if *s, uint8_t key) {
    /* Contains any op, but we set type as enum PRIM such that
       compiler warns if case statement is not total. */
    enum PRIM op;
    if (s->next) goto *(s->next);
  next:
    NEXT(s,op);
    switch(op) {
    case ACK: goto ack;
    case JSR: s->reg = C; s->cont = &&call; goto load_reg;
    case LDA: s->reg = A; s->cont = &&ack;  goto load_reg;
    case LDF: s->reg = F; s->cont = &&ack;  goto load_reg;
    case NPUSH:
        NEXT(s,s->count);
        while(s->count--) { DS--; NEXT(s,DS0); }
        goto ack;
    case NPOP:
        NEXT(s,s->count);
        while(s->count--) { cbuf_put(&s->out, DS0); DS++; }
        goto ack;
    case INTR:
        ASSERT(0);
    case NAL:
        NEXT(s,s->count);
        cbuf_put(&s->out, s->count);
        while(s->count--) { cbuf_put(&s->out, *(s->a)++); }
        goto next;
    case NFL:
        NEXT(s,s->count);
        cbuf_put(&s->out, s->count);
        while(s->count--) { cbuf_put(&s->out, *(s->f)++); }
        goto next;
    case NAS:
        NEXT(s,s->count);
        cbuf_put(&s->out, s->count);
        while(s->count--) { NEXT(s, *(s->a)++); }
        goto ack;
    case NFS:
        NEXT(s,s->count);
        while(s->count--) {
            // FIXME: Erase on boundary, write 16 bits at a time
        }
        goto ack;
    }
  load_reg:
    // Load 32 bits, little endian encoding
    s->count = 4;
    while(s->count--) {
        NEXT(s,s->tmp);
        uintptr_t *ptr = ((uintptr_t*)s) + s->reg;
        (*ptr) >>= 8;
        (*ptr) |= (s->tmp << 24);
    }
    goto *(s->cont);
  call:
    s->c();
    goto ack;
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
