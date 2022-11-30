/* Config. */
#define EMU
#define I2C_HALF_PERIOD_TICKS 1
#define LOG_I2C LOG

#include "cycle_counter.h"
#include <stdint.h>
#include "macros.h"

/* mod_i2c_info uses these. */
uint8_t info_byte_count;
int info_bytes(void) { return info_byte_count; }
int info_read(uint8_t *buf, uint32_t len) {
    uint32_t i=0;
    for(;;) {
        if (i >= len) break;
        if (!info_byte_count) break;
        buf[i++] = info_byte_count--;
    }
    return i;
}
#define INFOF_H

uint32_t cycle_counter_;
uint32_t cycle_counter() {
    return cycle_counter_;
}
/* Note that cycle_counter_expired() uses strict comparison.  See
   comment in that file.  To move past the timeout on the next tick of
   I2C_DELAY, we need to advance one more. */

void cycle_counter_next(void) {
    cycle_counter_ += 1 + I2C_HALF_PERIOD_TICKS;
}



/* Note that simulating the wire-and requires some effort.

   We need to keep track of the output state of each of the I2C ports,
   and combine the values on every read. */

struct i2c_port {
    uint8_t bits;
};
struct i2c_ports {
    struct i2c_port m, s;
} i2c_ports;
void i2c_ports_init(struct i2c_ports *p) {
    // Both high impedance.
    p->m.bits = 0b11;
    p->s.bits = 0b11;
}

int i2c_read_bus(void) {
    return i2c_ports.m.bits & i2c_ports.s.bits;
}
void i2c_write_port(struct i2c_port *port, uint8_t mask, int bitval) {
    port->bits &= ~mask;
    if (bitval) port->bits |= mask;
}


/* Tracker / slave. */
#include "gdb/mod_i2c_track.c"

/* Master impl + info packet write. */
#include "gdb/mod_i2c_bitbang.c"
#include "gdb/mod_i2c_info.c"

static inline int i2c_read_sda(struct i2c_port *s) {
    return !!(i2c_read_bus() & I2C_TRACK_SDA);
}
static inline int i2c_read_scl(struct i2c_port *s) {
    return !!(i2c_read_bus() & I2C_TRACK_SCL);
}
static inline void i2c_write_sda(struct i2c_port *s, int bitval) {
    i2c_write_port(s, I2C_TRACK_SDA, bitval);
}
static inline void i2c_write_scl(struct i2c_port *s, int bitval) {
    i2c_write_port(s, I2C_TRACK_SCL, bitval);
}



/* Logging:

   "t:" indicates state of bus right after running the master, and
   before running the slave. */

#include "macros.h"

#define LOG_ASSERT(cond) do { LOG(#cond "\n"); ASSERT(cond); } while(0)

struct i2c_track i2c_track = {};
struct i2c_info  i2c_info = {};

int main(void) {
    i2c_ports_init(&i2c_ports);
    i2c_track_init(&i2c_track, &i2c_ports.s);
    LOG("test: %s\n", __FILE__);
    //LOG_I2C("t: C D\nt: ---\n");
    info_byte_count = 0xff;

    for (int i=0; i<2; i++) {
        i2c_info_init(&i2c_info, &i2c_ports.m, 10);
        for(;;) {
            sm_status_t status1 = i2c_info_tick(&i2c_info);
            i2c_track.bus = i2c_read_bus();
            LOG_I2C("t: %d %d\n",
                    i2c_read_scl(&i2c_ports.m),
                    i2c_read_sda(&i2c_ports.m));
            sm_status_t status2 = i2c_track_tick(&i2c_track);
            ASSERT(SM_WAITING == status2);

            if (SM_WAITING != status1) break;

            ASSERT(cycle_counter() < 1000); // Infinite loop guard
            cycle_counter_next();
        }
    }
    return 0;
}
