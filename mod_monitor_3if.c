/* Staapl style 3-Instruction-Forth after attempting mod_forth_dsl.c
   which is not exactly what I need.

   See:
   - Staapl project pic18/interpreter.f
   - https://pages.cs.wisc.edu/~bolo/shipyard/3ins4th.html

   Differences to Stappl interpreter:
   - Codes are different
   - JSR doesn't load code ptr, LDC loads code ptr
   - Uses uintptr_t register lengths for ram, flash, code registers
   - Use an opaque framing mechanism, i.e. {packet,1}

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

#define MONITOR_3IF_FOR_PRIM(m)                              \
    m(ACK,  0x0)  m(NPUSH, 0x1)  m(NPOP, 0x2)  m(JSR,  0x3)  \
    m(LDA,  0x4)  m(LDF,   0x5)  m(LDC,  0x6)  m(INTR, 0x7)  \
    m(NAL,  0x8)  m(NFL,   0x9)  m(NAS,  0xa)  m(NFS,  0xb)  \

#define PRIM_ENUM_INIT(word,N) word = (0x80 + N),
enum PRIM { MONITOR_3IF_FOR_PRIM(PRIM_ENUM_INIT) };

struct monitor_3if;
struct monitor_3if {
    /* State used by code() called with JSR. */
    /* 0 */ uint8_t *ds;
    /* 1 */ void (*poll)(struct monitor_3if *s);
    /* 2 */ struct cbuf *out; // output buffer

    /* State used by host */
    /* 3 */ uint8_t *ram;
    /* 4 */ uint8_t *flash;
    /* 5 */ void (*code)(struct monitor_3if *s);

    /* Internal interpreter state */
    /* 6 */ void *next;  // continuation after byte input
    /* 7 */ void (*transfer)(struct monitor_3if *s);  // what to do inside loop
    /* 8 */ uint16_t count; uint8_t offset; uint8_t byte;

};

#define REG(name) OFFSETOF(struct monitor_3if, name)


#define DS (s->ds)
#define DS0 (DS[0])

/* This needs to be provided. */
#ifndef TO_FLASH_3IF
#define TO_FLASH_3IF(s)
#endif

/* Machine suspends on input (KEY), and output is buffered (EMIT). */

/* FIXME: Machine should suspend on output as well.  For now we assume
   the out buffer is always large enough, essentially moving flow
   control to the host end. */

/* Loop body loaded into s->code. */
void from_ram  (struct monitor_3if *s) { s->byte = *(s->ram)++; }
void from_flash(struct monitor_3if *s) { s->byte = *(s->flash)++; }

void to_ram    (struct monitor_3if *s) { *(s->ram)++ = s->byte; }
void to_flash  (struct monitor_3if *s) { TO_FLASH_3IF(s); }

void to_state  (struct monitor_3if *s) { ((uint8_t*)s)[s->offset++] = s->byte; }

/* The underlying Forth VM is fundamentally an 8-bit machine.  The
   orientation of the stack is such that multi-byte data has native
   byte order in the data coming FROM the host, and in the data stored
   in the stack memory.  This means that reading out TO the host will
   invert the data.  Note that it is possible to use from_ram with ram
   pointer pointing into stack to get proper byte order readout. */
void push_stack(struct monitor_3if *s) { *(s->ds)++ = s->byte; }
void pop_stack (struct monitor_3if *s) { s->byte = *--(s->ds); }


#ifndef MONITOR_3IF_BLOCKING

/* By default this implements a state machine written in push style,
   waiting for next byte.  Each occurrence of NEXT is a suspend
   point. */
#define NEXT_LABEL(s,var,label)                         \
    do {						\
	s->next = &&label;				\
	return 0;                                       \
      label: 						\
        (var) = key;                                    \
    } while(0)
#define NEXT(s,var)                             \
    NEXT_LABEL(s,var,GENSYM(label_))
#else
/* Alternatively the main loop can be hosted in a thread with
   blocking read / write. */
uint8_t monitor_3if_read_byte(struct monitor_3if *);
#define NEXT(s,var) do { key = monitor_3if_read_byte(s); (var) = key; } while(0)
#endif


#ifndef MONITOR_3IF_LOG
#define MONITOR_3IF_LOG(...)
#endif

static inline uintptr_t monitor_3if_push_key(struct monitor_3if *s, uint8_t key) {
    enum PRIM op;
#ifndef MONITOR_3IF_BLOCKING
    if (s->next) goto *(s->next);
#endif
  next:
#ifndef MONITOR_3IF_BLOCKING
    // FIXME: Flush output here, or create a dedicated opcode.
#endif
    NEXT(s,s->count);
    if (s->count == 0) {
        // 0-size packets are treated as NOP in most uc_tools
        // encodings (notably SLIP) and are often used as heartbeat /
        // sync, so send a 0-size reply
        cbuf_put(s->out, 0);
        goto next;
    }
    s->poll = NULL;

    NEXT(s,op);
    MONITOR_3IF_LOG(s, "n=%d, op=%02x\n", s->count, op);
    // count contains nb bytes after length byte
    // the first byte is the opcode
    s->count--;
    // count now countains nb bytes of payload for opcode
    switch(op) {

    case ACK: if (s->count != 0) goto err; else goto ack;

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

  err:
    // LOG("err\n");
    return 1;

  loop_from:
    // nb bytes after opcode
    if (s->count != 1) goto err;
    // needs s->transfer
    NEXT(s,s->count);
    cbuf_put(s->out, s->count);
    while(s->count--) { s->transfer(s); cbuf_put(s->out, s->byte); }
    goto next;

  to_reg:
    // needs s->offset
    s->transfer = to_state;
    if (s->count != sizeof(uintptr_t)) goto err;
  loop_to:
    // needs s->transfer
    while(s->count--) { NEXT(s, s->byte); s->transfer(s); }
  ack:
    // 0-size packets are NOP, so send a non-zero size ack
    cbuf_put(s->out, 1);
    cbuf_put(s->out, 0);
    // if poll was set (e.g. by JSR) we can call it after ack
    if (s->poll) s->poll(s);
    goto next;
}

#ifndef MONITOR_3IF_BLOCKING
uintptr_t monitor_3if_write(struct monitor_3if *s,
                            const uint8_t *buf, uint32_t len) {
    for (uint32_t i=0; i<len; i++) {
        uintptr_t status = monitor_3if_push_key(s, buf[i]);
        if (status) return status;
    }
    return 0;
}
#endif

void monitor_3if_init(struct monitor_3if *s,
                      struct cbuf *out, uint8_t *ds_buf) {
    memset(s,0,sizeof(*s));
    cbuf_clear(out);
    s->out = out;
    s->ds = ds_buf;
#ifndef MONITOR_3IF_BLOCKING
    /* Dummy write to end up in the first blocking read. */
    uint8_t dummy = 0; monitor_3if_write(s, &dummy, 1);
#endif
}


/* About poll:

   The poll function can be called when it is non-zero, e.g. right
   before USB polls the reply buffer.  It can be set by a function
   called through JSR.

   Any commands sent to the monitor will need to disable the poll
   routine for two reasons: 1. we don't want events generated by poll
   to collide with monitor replies which are not tagged, and 2. the
   pointer might be invalidated by writing to RAM or Flash.

   The poll() function is allowed to put any event data in the output
   buffer in {packet,1} format except for the ack response, which is a
   packet containing a single zero byte.

   The handshake works as follows:

   ( suppose poll was enabled and there are events in the buffer )

   1. Host wants to send a series of commands.  It sends an ACK
      command.  When monitor sees ack it will disable poll so no more
      async messages will be supported.

   2. Host reads messages until it receives the ACK reply.

   3. At this point host can send multiple requests.  The poll()
      routine will be off and no async messages will be produced.

   4. Host can send a JSR command that will run code that enables
      poll.  As long as host does not send any further messages, the
      poll() routine will remain active and it can be used to produce
      async messages, e.g. called from USB poll routine.

   Note that poll() needs to leave room for the ACK reply sent by 3if
   monitor.  Otherwise protocol will get stuck.


   The poll() mechanism was added in this way to keep a strict
   synchronous request/response protocol as the base which is all that
   is needed for firmware update.

   The poll() mechanism allows implementation of a bi-directional
   message bridge in the following way: host uploads a handler
   function that will enable a poll routine.  Host then waits for any
   notifications coming from target.  In response to a notification it
   will read messages from a memory buffer, and runs the handler again
   to re-enable a poll routine.  If host needs to send a message, it
   can place it into a buffer and pass a parameter to the handler
   routine to further handle the message and re-enable poll again.

*/


#endif
