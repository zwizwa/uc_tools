/* Test application for 3if monitor run-from-RAM JSR and poll()
   functionality. */

#include <stdint.h>
#include "cbuf.h"
struct run_3if;
struct run_3if {
    uint8_t *ds;
    void (*poll)(struct run_3if *);
    struct cbuf *out;
};
void    push (struct run_3if *s, uint8_t val) { *(s->ds)++ = val; }
uint8_t pop  (struct run_3if *s) { return *--(s->ds); }

#include "hw_stm32f103.h"
#include "uct_byteswap.h"

#define OUT_RESERVE 2

uint8_t data[1000] = {1,2,3};

/* The purpose of poll() is to put a notification in s->out if there
   is room.  Notifications are intended to be small; think of them as
   interrupts.  Host will perform action in response, e.g. read out
   buffer from target memory. */
void poll(struct run_3if *s) {

    // It seems that if the total number of bytes written here is > 64
    // we get into trouble.

    for (uint32_t i=0; i<1; i++) {
        uint8_t msg[65] = {
            0,         // nb bytes after size byte (patched below)
            0xFF,      // nonzero tag to distinguish from 00 ack reply
            U32_LE((uint32_t)data),
            // 1,1,1,1,1,
            // 2,2,2,2,2,
        };
        msg[0] = sizeof(msg) - 1;
        uint32_t room = cbuf_room(s->out);
        if (room < sizeof(msg) + OUT_RESERVE) return;
        cbuf_write(s->out, msg, sizeof(msg));
    }
}

/* Goes into a separate section so linker script can place it into a
   predictable location, e.g. start of RAM segment. */
__attribute__((section(".run")))
int run(struct run_3if *s) {
    s->poll = poll;
    return 0;
}
