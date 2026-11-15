/* Fake 8 port DMX at 2Msps on stdout, real time.  Test input for la_gl.c
   Overview: see la_gl.c
   Each port repeats a fixed frame: break, mark, start code, slots.
   Frame length and idle time differ per port. */

#include "macros.h"
#include "assert_write.h"
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#define RATE  2000000
#define DIV   8          // samples per bit, 250kBaud
#define BLOCK (RATE/100) // 10ms

struct port {
    uint8_t *bit;
    uint32_t nb, i;
};

static void bits(struct port *p, int level, int nb) {
    while (nb--) p->bit[p->nb++] = level;
}
static void slot(struct port *p, uint8_t byte) {
    bits(p, 0, DIV);
    for (int i=0; i<8; i++) bits(p, 1 & (byte >> i), DIV);
    bits(p, 1, 2*DIV);
}
static void port_init(struct port *p, int n) {
    int nb_slots = 8 + 4*n;
    int idle = (RATE/1000) * 20 * (n+1);
    p->bit = malloc(1000 + (1 + nb_slots) * 11 * DIV + idle);
    ASSERT(p->bit);
    bits(p, 0, 200); // break, 100us
    bits(p, 1, 24);  // mark after break, 12us
    slot(p, 0);      // start code
    for (int i=0; i<nb_slots; i++) slot(p, (n << 5) + i);
    bits(p, 1, idle);
}

int main(void) {
    struct port port[8] = {};
    for (int n=0; n<8; n++) port_init(&port[n], n);
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    for(;;) {
        uint8_t buf[BLOCK];
        for (int i=0; i<BLOCK; i++) {
            uint8_t s = 0;
            for (int n=0; n<8; n++) {
                struct port *p = &port[n];
                s |= p->bit[p->i] << n;
                if (++p->i == p->nb) p->i = 0;
            }
            buf[i] = s;
        }
        assert_write(1, buf, sizeof(buf));
        t.tv_nsec += 10000000;
        if (t.tv_nsec >= 1000000000) { t.tv_nsec -= 1000000000; t.tv_sec++; }
        clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &t, NULL);
    }
}
