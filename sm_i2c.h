#ifndef SM_I2C_H
#define SM_I2C_H

/* Run I2C command in polling state machine, by wrapping hw_i2c.h read
   and write calls. */

#include "hw_i2c.h"

#include "sm.h"
struct sm_i2c_transmit {
    void *next;
}
void sm_i2c_transmit_init(struct sm_i2c *s) {
}
void sm_i2c_transmit_tick(struct sm_i2c *s) {
}




#endif
