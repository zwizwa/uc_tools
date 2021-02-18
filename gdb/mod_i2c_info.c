#ifndef MOD_I2C_INFO
#define MOD_I2C_INFO

/* This currently only works with the bitbang implementation, as it
   exposes a byte write.  If hw i2c is enabled this will cause an
   error. */
#include "mod_i2c_bitbang.c"

struct i2c_info {
    void *next;
    int32_t left;
    union {
        struct i2c_start_state     i2c_start;
        struct i2c_send_byte_state i2c_send_byte;
        struct i2c_stop_state      i2c_stop;
    } sub;
};
void i2c_info_init(struct i2c_info *s, uint32_t left) {
    ZERO(s);
    s->left = left;
}

// what to pick here?
#define I2C_INFO_ADDR 0x10

#ifndef LOG_I2C
#define LOG_I2C(...)
#endif

/* This intentionally does not produce errors.  It logs only when
   there is a receiver available on the bus.  If it gets a nack, it
   just stops.  Typically it will get the nack on the address byte, so
   nothing is taken from the buffer. */

sm_status_t i2c_info_tick(struct i2c_info *s) {
    SM_RESUME(s);
    SM_SUB_CATCH0(s, i2c_start);
    if (SM_SUB_CATCH(s, i2c_send_byte, I2C_INFO_ADDR << 1)) {
        LOG_I2C(" nack@start");
        goto done;
    }
    // FIXME: should this throttle? also, watch out for log feedback
    while ((info_bytes()) > 0 && (s->left-- > 0)) {
        uint8_t byte;
        info_read(&byte, 1);
        if (SM_SUB_CATCH(s, i2c_send_byte, byte)) {
            LOG_I2C(" nack@%d", s->left);
            goto done;
        }
    }
  done:
    SM_SUB_CATCH0(s, i2c_stop);
    SM_HALT(s);
}


#endif
