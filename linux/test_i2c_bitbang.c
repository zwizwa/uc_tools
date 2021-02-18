/* Config. */
#define EMU
#define I2C_HALF_PERIOD_TICKS 1
#define LOG_I2C LOG

#include "cycle_counter.h"
#include <stdint.h>
#include "macros.h"

#if 0
#include "info_null.c"
#else
uint8_t byte_count;
int info_bytes(void) { return 1; }
int info_read(uint8_t *buf, uint32_t len) {
    for (uint32_t i=0; i<len; i++) {
        buf[i] = byte_count++;
    }
    return len;
}

#endif


void i2c_write_sda(int bitval);
void i2c_write_scl(int bitval);
int i2c_read_sda(void);
int i2c_read_scl(void);

void notify(void);
uint32_t cycle_counter_;
uint32_t cycle_counter() {
    return cycle_counter_;
}
void cycle_counter_next(void) {
    cycle_counter_++;
}
// FIXME: these will eventually be platform-independent, so move them up
#include "gdb/mod_i2c_bitbang.c"
#include "gdb/mod_i2c_info.c"
#include "gdb/mod_i2c_track.c"

struct i2c_track i2c_track;
struct i2c_info  i2c_info;

void i2c_write_sda(int bitval) {
    i2c_track.bus &= ~I2C_TRACK_SDA;
    if (bitval) i2c_track.bus |= I2C_TRACK_SDA;
    notify();
}
void i2c_write_scl(int bitval) {
    i2c_track.bus &= ~I2C_TRACK_SCL;
    if (bitval) i2c_track.bus |= I2C_TRACK_SCL;
    notify();
}

int i2c_read_sda(void) {
    return !!(i2c_track.bus & I2C_TRACK_SDA);
}
int i2c_read_scl(void) {
    return !!(i2c_track.bus & I2C_TRACK_SCL);
}

void notify(void) {
}


#include "macros.h"
int main(void) {
    i2c_track_init(&i2c_track);
    LOG("test: %s\n", __FILE__);
    i2c_info_init(&i2c_info, 10);
    LOG_I2C("C D\n---\n");
    for(;;) {
        sm_status_t status1 = i2c_info_tick(&i2c_info);
        LOG_I2C("%d %d ", i2c_read_scl(), i2c_read_sda());
        sm_status_t status2 = i2c_track_tick(&i2c_track);
        ASSERT(SM_WAITING == status2);

        if (SM_WAITING != status1) break;
        LOG_I2C("\n");

        ASSERT(cycle_counter() < 100000); // Infinite loop guard
        cycle_counter_next();
    }
    return 0;
}
