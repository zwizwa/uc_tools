#ifndef SM_I2C_H
#define SM_I2C_H

/* Run I2C command in polling state machine, by wrapping hw_i2c.h read
   and write calls.

   To use this abstraction, I2C drivers need to be written in a way
   that they can generate a command template, which is then executed
   by this machine.

   Note that ordinary function composition will not work to abstract
   I2C RPC calls.  The composition mechanism will need to be that of
   sm macros (SM_CALL).

   This means that drivers should focus on creating some kind of
   command structure, that is then executed by an interpreter written
   as an sm.h state machine.

*/

#include "hw_i2c.h"

#include "sm.h"
#include "hw_i2c.h"
struct sm_i2c {
    void *next;
    struct hw_i2c_transmit_state transmit;
};
uint32_t sm_i2c_transmit_init(struct sm_i2c *s, struct hw_i2c c) {
    return 0;
}
uint32_t sm_i2c_transmit_tick(struct sm_i2c *s, struct hw_i2c c) {
#define S (&(s->transmit))
#include "hw_i2c_transmit_body.h"
#undef S
}




#endif
