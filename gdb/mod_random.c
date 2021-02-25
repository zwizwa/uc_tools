#ifndef MOD_RANDOM
#define MOD_RANDOM

/* Pseudo random number sequence.

   This uses the CRC of the device UID to seed, so is deterministic
   but unique for each device. */

#include "instance.h"
#define XORSHIFT_STATIC 0
#include "xorshift.h"
#include "crc.h"

uint32_t random_state;
uint32_t random_u32(void) {
    return srandom_u32(&random_state);
}
instance_status_t random_init(instance_init_t *ctx) {
#ifdef HW_DEVICE_SERIAL
    const uint8_t *stm_id = HW_DEVICE_SERIAL;
    /* The 1 + is to avoid the slim chance that crc is 0.
       A 0 seed for xorshift prng will produce a sequence of 0s. */
    random_state = 1 + crc32b(stm_id, HW_DEVICE_SERIAL_LENGTH);
#endif
    return 0;
}
DEF_INSTANCE(random);

#endif
