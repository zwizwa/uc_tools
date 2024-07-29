#ifndef MOD_I2C_TRACK
#define MOD_I2C_TRACK

/* I2C tracker / slave

   This needs to respond immediately so it should probably run from
   interrupt, e.g. the EXTI mechanism.

   The state machine is implemented in sm_cond.h conditional style,
   which means it will always always evaluate the state of the pins
   and will not rely on events passed in by the interrupt mechanism.
   I.e. the state machine is essentially just polling mode, and the
   events are used to poll on _possible_ state change.
*/

#include "sm.h"
#include "pbuf.h"

/* The intention here is to create trace logs that interleave properly
   with the test driver. */
#ifndef I2C_TRACK_LOG
#error need I2C_TRACK_LOG
// #define I2C_TRACK_LOG(...)
#endif

// 3.0 Abstract machine

struct i2c_port;
static inline void i2c_write_sda(struct i2c_port *p, int v);
static inline void i2c_write_scl(struct i2c_port *p, int v);

// This machine as a different API: relies on bus values to be passed in
static inline int  i2c_read_sda(struct i2c_port *p);
static inline int  i2c_read_scl(struct i2c_port *p);

#define I2C_TRACK_SCL (1<<1)
#define I2C_TRACK_SDA (1<<0)

#define I2C_WAIT_C1D1_IDLE(s)  SM_WAIT(s,   s->bus_bits == 0b11)
#define I2C_WAIT_C1D0_START(s) SM_WAIT(s,   s->bus_bits == 0b10)
#define I2C_WAIT_C1Dx_HI(s)    SM_WAIT(s,   s->bus_bits &  0b10)
#define I2C_WAIT_C0Dx_LO(s)    SM_WAIT(s, !(s->bus_bits &  0b10))

#define I2C_FLAG_TRANSMIT (1 << 0)

struct i2c_track;
struct i2c_track {
    void *next;
    struct i2c_port *i2c_port;
    struct pbuf *p;
    uint16_t sreg;     // shift register (8 x data + ack)
    uint16_t byte;     // byte count
    uint8_t bit;       // bit count
    uint8_t bus_bits;  // current bus value
    uint8_t bus_bits0; // previous bus value (for STOP detection)
    uint8_t flags;
};
struct i2c_track i2c_track;
void i2c_track_init(struct i2c_track *s, struct i2c_port *i2c_port) {
    memset(s,0,sizeof(*s));
    s->i2c_port = i2c_port;
    s->bus_bits = 0b11;
}
// CD -> CD
// 11    10  start
// 10    11  stop
// 1_    0_  write edge
// 0_    1_  read edge

// Machine, in the abstract:
// wait start
// wait read edge, read x 8
// wait write edge, write ack
//
// so machine can just wait on a condition (DC combo), we don't
// need to track history, that is implicit in state machine.

// FIXME: This still needs some work.
// 1. Convert the inner machine to a byte read/write machine similar to bitbang.
// 2. Focus on getting test_i2c_bitbang.c to work properly first before moving this to uc.

/* The code is structured as a skeleton of two nested loops for bytes
   and bits with suspension points in the inner loop. Dispatch points
   are then inserted for actions that need to be taken at particular
   bit or byte counts.  That is simpler to express than "inverted".

   Terminology used (master read/write vs. receive/transmit)
   - we receive  during master data or address write
   - we transmit during master data read
*/

uint32_t i2c_track_poll(struct i2c_track *s) {
    s->bus_bits =
        (i2c_read_sda(s->i2c_port) * I2C_TRACK_SDA) +
        (i2c_read_scl(s->i2c_port) * I2C_TRACK_SCL);

    SM_RESUME(s);
    I2C_TRACK_LOG(s, "s: task start\n");

    I2C_WAIT_C1D1_IDLE(s);

  stop:
    for(;;) {
        I2C_WAIT_C1D0_START(s);
        /* Start is defined as SDA 1->0 when SCL == 1. */
        I2C_TRACK_LOG(s, "s: start\n");

        /* The first byte is address byte, we're always in read mode
           and will acknowledge. */

        for(s->byte = 0; /* see goto stop */; s->byte++) {
            /* Note that read and write can be implemented in the same
               loop, since a write of a 1 bit is a no-op, so the loop
               always contains a write. */

            // FIXME: repeated start?

            // Byte frame: 8 data bits + 1 ack bit.
            for(s->bit = 0; s->bit < 9; s->bit++) {

                I2C_WAIT_C0Dx_LO(s); // when C=0 write is allowed

                if (s->bit == 8) {
                    // The ack bit depends on a couple of things.
                    if (s->flags & I2C_FLAG_TRANSMIT) {
                        // In transmit mode, we release the line since
                        // other end needs to acknowledge.
                        i2c_write_sda(s->i2c_port,1);
                    }
                    else if (s->byte == 0) {
                        // Inaddress receive mode, we only acknowledge
                        // if the address matches.
                        uint16_t receive = s->sreg & 1;
                        uint16_t addr    = (s->sreg >> 1) & 0x7f;
                        I2C_TRACK_LOG(s, "s: addr = 0x%02x\n", addr);
                        if (0x10 == addr) { // FIXME
                            i2c_write_sda(s->i2c_port,0); // ack
                        }
                        else {
                            i2c_write_sda(s->i2c_port,1); // nack
                        }
                        (void)receive; // FIXME
                    }
                    else {
                        // In data receive mode there are a number of
                        // conditions that can cause a nack, but we
                        // will just acknowledge here.
                        i2c_write_sda(s->i2c_port,0);
                    }
                }
                else {
                    // The data bit depends on what mode we're in.
                    if (s->flags & I2C_FLAG_TRANSMIT) {
                        i2c_write_sda(s->i2c_port,1);  // FIXME: actual data
                    }
                    else {
                        // Release the line in receive mode.
                        i2c_write_sda(s->i2c_port,1);
                    }
                }

                I2C_WAIT_C1Dx_HI(s); // when C=1 data is stable
                s->sreg = (s->bus_bits & 1) | (s->sreg << 1);

                // I2C_TRACK_LOG("s: b%d=%d\n", s->bit, s->bus_bits & 1);

                // To distinguish between normal clock transition and
                // STOP, we wait for a change and inspect it.
                s->bus_bits0 = s->bus_bits;
                SM_WAIT(s, s->bus_bits != s->bus_bits0);

                // SDA 0->1 while SCL=1 is a STOP condition.  It
                // always indicates end of transmission, even in the
                // middle of a byte.
                if ((s->bus_bits0 == 0b10) &&
                    (s->bus_bits  == 0b11)) {
                    I2C_TRACK_LOG(s, "s: stop\n");
                    goto stop;
                }
                // SCL 1->0 with SDA=dontcare indicates a normal clock
                // edge, indicating a new bit is coming and the loop
                // can continue.
            }
            // LSB is the ACK bit.
            uint16_t byte = (s->sreg >> 1) & 0xFF;
            uint16_t ack  = s->sreg & 1;
            (void)ack;
            I2C_TRACK_LOG(s, "s: 0x%02x %d\n", byte, ack);
            // Keep track of bytes.
            if (s->p) pbuf_put(s->p, byte);
        }
    }
}



#endif

