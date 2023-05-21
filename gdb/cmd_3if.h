#ifndef CMD_3IF_H
#define CMD_3IF_H

/* Interface to 3if monitor, firmware application side.  This allows
   defining commands that operate as extensions of the 3if monitor.
   Note that 2 call sequences exists: JSR (implicit ack for wrapping
   ordinary C functions) and INTR (command produces response == a true
   extension).  Note that commands specified in the gdbstub_config are
   of the INTR kind. */

#include <stdint.h>
#include "cbuf.h"

/* Overlay of the public part of the 3if state. */
struct cmd_3if;
struct cmd_3if {
    /* Data stack for parameter passing between target and host.  */
    uint8_t *ds;
    /* The purpose of poll() is to put a notification in s->out if
       there is room.  Notifications are intended to be small; think
       of them as interrupts.  Host will perform action in response,
       e.g. read out buffer from target memory. */
    void (*poll)(struct cmd_3if *);
    struct cbuf *out;
};
static inline void cmd_3if_push (struct cmd_3if *s, uint8_t val) {
    *(s->ds)++ = val;
}
static inline uint8_t cmd_3if_pop  (struct cmd_3if *s) {
    return *--(s->ds);
}

// FIXME: This is necessary for RAM.  Change the linker file.
#define CMD_3IF_SECTION __attribute__((section(".run")))


#endif
