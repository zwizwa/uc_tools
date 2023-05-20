#ifndef RUN_3IF_H
#define RUN_3IF_H

/* Interface to 3if monitor, application side, used to run an
   application as an extension of the 3if monitor. */

#include <stdint.h>
#include "cbuf.h"

/* Overlay of the public part of the 3if state. */
struct run_3if;
struct run_3if {
    /* Data stack for parameter passing between target and host.  */
    uint8_t *ds;
    /* The purpose of poll() is to put a notification in s->out if
       there is room.  Notifications are intended to be small; think
       of them as interrupts.  Host will perform action in response,
       e.g. read out buffer from target memory. */
    void (*poll)(struct run_3if *);
    struct cbuf *out;
};
static inline void run_3if_push (struct run_3if *s, uint8_t val) {
    *(s->ds)++ = val;
}
static inline uint8_t run_3if_pop  (struct run_3if *s) {
    return *--(s->ds);
}

#define RUN_3IF_SECTION __attribute__((section(".run")))


#endif
