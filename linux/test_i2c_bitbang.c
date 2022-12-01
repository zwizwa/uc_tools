/* Config. */
#define EMU
#define I2C_HALF_PERIOD_TICKS 1
#define I2C_LOG LOG

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
uint32_t cycle_counter(void) {
    return cycle_counter_;
}
/* Note that cycle_counter_expired() uses strict comparison.  See
   comment in that file.  To move past the timeout on the next tick of
   I2C_DELAY, we need to advance one more. */

void cycle_counter_next(void) {
    cycle_counter_ += 1 + I2C_HALF_PERIOD_TICKS;
}
struct i2c_port;
static inline uint32_t i2c_port_future_time(struct i2c_port *s, uint32_t ticks) {
    return cycle_counter_future_time(ticks);
}
static inline uint32_t i2c_port_timeout_expired(struct i2c_port *s, uint32_t timeout) {
    return cycle_counter_expired(timeout);
}




/* Tracker / slave. */
#define I2C_TRACK_LOG(s, ...) LOG(__VA_ARGS__)
#include "gdb/mod_i2c_track.c"

/* Master impl + info packet write. */
#include "gdb/mod_i2c_bitbang.c"
#include "gdb/mod_i2c_info.c"

/* I2C bus with 2 ports. */
#include "mod_test_i2c_ms.c"




/* Logging:

   "t:" indicates state of bus right after running the master, and
   before running the slave. */

#include "macros.h"

#define LOG_ASSERT(cond) do { LOG(#cond "\n"); ASSERT(cond); } while(0)

struct i2c_track i2c_track = {};
struct i2c_info  i2c_info = {};

void test_v1(void) {

    i2c_ports_init(&i2c_ports);
    i2c_track_init(&i2c_track, &i2c_ports.s);
    LOG("test: %s\n", __FILE__);
    //I2C_LOG("t: C D\nt: ---\n");
    info_byte_count = 0xff;

    for (int i=0; i<2; i++) {
        i2c_info_init(&i2c_info, &i2c_ports.m, 10);
        for(;;) {
            sm_status_t status1 = i2c_info_tick(&i2c_info);
            I2C_LOG("t: %d %d\n",
                    i2c_read_scl(&i2c_ports.m),
                    i2c_read_sda(&i2c_ports.m));
            sm_status_t status2 = i2c_track_poll(&i2c_track);
            ASSERT(SM_WAITING == status2);

            if (SM_WAITING != status1) break;

            ASSERT(cycle_counter() < 1000); // Infinite loop guard
            cycle_counter_next();
        }
    }
}

/* FIXME: The v1 implementation uses undefined behavior in the macros
   (can't mix statement expressions and computed goto).  There is a v2
   re-implementation which is just a collection of macros and only
   used in proprietary app.  Make a test that emulates an eeprom. */
void test_v2(void) {
}


int main(void) {
    test_v1();
    test_v2();
}
