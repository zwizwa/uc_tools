#ifndef MOD_I2C_EEPROM
#define MOD_I2C_EEPROM

/* I2C eeprom
   Modeled after mod_i2c_track.c

   This needs to respond immediately so it should probably run from
   interrupt, e.g. the EXTI mechanism.

   The state machine is implemented in sm_cond.h conditional style,
   which means it will always always evaluate the state of the pins on
   re-entry, and will not rely on events passed in by the interrupt
   mechanism.  I.e. the state machine is essentially just polling
   mode, and the events are used to poll on _possible_ state change.
*/

#include "sm.h"
#include "pbuf.h"

/* The intention here is to create trace logs that interleave properly
   with the test driver. */
#ifndef I2C_EEPROM_LOG
#error need I2C_EEPROM_LOG
// #define I2C_EEPROM_LOG(...)
#endif

// 3.0 Abstract machine

struct i2c_port;
static inline void i2c_write_sda(struct i2c_port *p, int v);
static inline void i2c_write_scl(struct i2c_port *p, int v);

// This machine as a different API: relies on bus values to be passed in
static inline int  i2c_read_sda(struct i2c_port *p);
static inline int  i2c_read_scl(struct i2c_port *p);

#define I2C_EEPROM_SCL (1<<1)
#define I2C_EEPROM_SDA (1<<0)

#define I2C_WAIT_C1D1_IDLE(s)  SM_WAIT(s,   s->bus_bits == 0b11)
#define I2C_WAIT_C1D0_START(s) SM_WAIT(s,   s->bus_bits == 0b10)
#define I2C_WAIT_C1Dx_HI(s)    SM_WAIT(s,   s->bus_bits &  0b10)
#define I2C_WAIT_C0Dx_LO(s)    SM_WAIT(s, !(s->bus_bits &  0b10))

struct i2c_eeprom;
struct i2c_eeprom {
    void *next;
    struct i2c_port *i2c_port;
    struct pbuf *p;
    uint16_t sreg;     // shift register (8 x data + ack)
    uint16_t byte;     // byte count
    uint8_t bit;       // bit count
    uint8_t bus_bits;  // current bus value
    uint8_t bus_bits0; // previous bus value (for STOP detection)
    uint8_t cur_addr;  // currently addressed
    uint8_t offset;
    unsigned int receive:1;
    uint8_t eeprom[256];
};
struct i2c_eeprom i2c_eeprom;
void i2c_eeprom_init(struct i2c_eeprom *s, struct i2c_port *i2c_port) {
    memset(s,0,sizeof(*s));
    s->i2c_port = i2c_port;
    s->bus_bits = 0b11;
    memset(s->eeprom,0xFF,sizeof(s->eeprom));
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
// need to eeprom history, that is implicit in state machine.

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

#define EEPROM_ADDR 0x54

uint32_t i2c_eeprom_poll(struct i2c_eeprom *s) {
    s->bus_bits =
        (i2c_read_sda(s->i2c_port) * I2C_EEPROM_SDA) +
        (i2c_read_scl(s->i2c_port) * I2C_EEPROM_SCL);

    SM_RESUME(s);
    I2C_EEPROM_LOG(s, "s: task start\n");

    I2C_WAIT_C1D1_IDLE(s);

  stop:
    for(;;) {
        /* The I2C data direction bit.
           0 means M->S
           1 means S->M */
        s->receive = 0;   // first byte is always m->s
        s->cur_addr = 0;  // not yet addressed

        I2C_WAIT_C1D0_START(s);
        /* Start is defined as SDA 1->0 when SCL == 1. */
        I2C_EEPROM_LOG(s, "s: start\n");

        /* The first byte is address byte, we're always in read mode
           and will acknowledge. */



        for(s->byte = 0; /* see goto stop */; s->byte++) {
            // Byte frame: 8 data bits + 1 ack bit.
            for(s->bit = 0; s->bit < 9; s->bit++) {

                I2C_WAIT_C0Dx_LO(s); // when C=0 write is allowed

                if (s->bit == 8) { // Ack bit
                    if (s->byte == 0) {
                        // Set s->cur_addr and s->receive from first byte
                        s->receive  = s->sreg & 1;
                        s->cur_addr = (s->sreg >> 1) & 0x7f;
                        I2C_EEPROM_LOG(s, "s: addr = 0x%02x\n", s->cur_addr);
                        if (EEPROM_ADDR == s->cur_addr) {
                            i2c_write_sda(s->i2c_port,0); // ack
                        }
                        else {
                            i2c_write_sda(s->i2c_port,1); // nack
                        }
                    }
                    else {
                        // Use s->cur_addr and s->receive on subsequent bytes
                        if (s->receive || EEPROM_ADDR != s->cur_addr) {
                            i2c_write_sda(s->i2c_port,1); // nack
                        }
                        else {
                            // Addressed and master is transmitting,
                            // so we as slave have to ack the data.
                            i2c_write_sda(s->i2c_port,0); // ack
                        }
                    }
                }
                else { // Data bit
                    if (s->receive && (EEPROM_ADDR == s->cur_addr)) {
                        // Assert line when master is receiving
                        int data_bit = (s->eeprom[s->offset] >> (7 - s->bit)) & 1;
                        i2c_write_sda(s->i2c_port,data_bit);
                    }
                    else {
                        // Release line so master can transmit
                        i2c_write_sda(s->i2c_port,1);
                    }
                }

                I2C_WAIT_C1Dx_HI(s); // when C=1 data is stable
                s->sreg = (s->bus_bits & 1) | (s->sreg << 1);

                // I2C_EEPROM_LOG("s: b%d=%d\n", s->bit, s->bus_bits & 1);

                // To distinguish between normal clock transition and
                // STOP, we wait for a change and inspect it.
                s->bus_bits0 = s->bus_bits;
                SM_WAIT(s, s->bus_bits != s->bus_bits0);

                // SDA 0->1 while SCL=1 is a STOP condition.  It
                // always indicates end of transmission, even in the
                // middle of a byte.
                if ((s->bus_bits0 == 0b10) &&
                    (s->bus_bits  == 0b11)) {
                    I2C_EEPROM_LOG(s, "s: stop\n");
                    goto stop;
                }
                // SCL 1->0 with SDA=dontcare indicates a normal clock
                // edge, indicating a new bit is coming and the loop
                // can continue.
            }
            // LSB is the ACK bit.
            uint16_t data = (s->sreg >> 1) & 0xFF;
            uint16_t ack  = s->sreg & 1;
            (void)ack;
            I2C_EEPROM_LOG(s, "s: 0x%02x ack=%d byte=%d\n", data, ack, s->byte);
            if (s->byte > 0 && (EEPROM_ADDR == s->cur_addr)) {
                if (s->receive) {
                    // We are transmitting
                    s->offset++;
                }
                else {
                    // Master is transmitting
                    if (s->byte == 1) {
                        // First data byte is offset
                        I2C_EEPROM_LOG(s, "s: offset=%d\n", data);
                        s->offset = data;
                    }
                    else {
                        I2C_EEPROM_LOG(s, "s: write 0x%02x at %d\n", data, s->offset);
                        s->eeprom[s->offset++] = data;
                    }
                }
            }
        }
    }
}



#endif

