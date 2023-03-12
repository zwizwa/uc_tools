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

void poll(struct run_3if *s) {
    uint8_t msg[] = {1, 0xFF};
    cbuf_write(s->out, msg, sizeof(msg));
    s->poll = NULL;
}

/* Goes into a separate section so linker script can place it into a
   predictable location, e.g. start of RAM segment. */
__attribute__((section(".run")))
int run(struct run_3if *s) {
    //uint8_t relays = pop(s);
    s->poll = poll;
    return 0;
}
