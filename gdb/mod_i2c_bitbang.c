#ifndef MOD_I2C_BITBANG
#define MOD_I2C_BITBANG

/* I2C Master bitbang implementation. */
#include "mod_i2c_bitbang_macros.c"


#include "sm.h"
#include "sm_def.h"
#include "slice.h"



/* By default this is off.  It is defined e.g. in the host and target
   test applications or can be enabled for ad-hoc debugging. */
#ifndef I2C_LOG
#define I2C_LOG(...)
#endif


/* A note on design: the async generators / protothreads / state
   machines (sm.h) rely on computed goto for suspension, and cannot be
   composed using ordinary function calls.  E.g. you can't call a
   function that would block.

   But, you can use: the SM_SUB mechanism to "call" another state
   machine, initializing it and running it until it is no longer
   SM_WAITING.  Semanticially this "sub machine" behaves the same as a
   subroutine, but the C boilerplate is a bit more heavy.

   That is why we also use macros to abstract some code snippets that
   then get inlined into the main body of the protothread.

   In short: there are two abstraction mechanisms.  How to choose
   betwen them is a matter of taste.  The guideline I use is to split
   things up into separate sub machines when there is a lot of re-use,
   and to use macros when you just want to split things up for
   readability, or when the code that is generated by the macro is
   small (e.g. I2C_DELAY).

*/

#include "cycle_counter.h"

/* Synchronization primitives: time delay to implement the clock, and
   i/o condition poll to implement clock stretching by slave.  These
   will "infect" all other code that is written in terms of them. */

/* This can be used to move the blocking loop into the inner loop, for
   testing. */
// #define I2C_WHILE(s,cond) while(cond)
#define I2C_WHILE(s,cond) SM_WHILE(s,cond)

#define I2C_WAIT SM_WAIT





/* Deblock is most useful right before start, so inline it into the
   start state machine. */
struct i2c_start_state {
    void *next;
    struct i2c_port *port;
    uint32_t timeout; // for I2C_DELAY macro
    int8_t clock; // for I2C_DEBLOCK
};
void i2c_start_init(struct i2c_start_state *s, struct i2c_port *port) {
    s->next = 0;
    s->port = port;
}
sm_status_t i2c_start_tick(struct i2c_start_state *s) {
    SM_RESUME(s);
    I2C_DEBLOCK(s);
    I2C_START(s);
    SM_HALT(s);
}

void i2c_info_busy(void *s, const char *tag) {
    if (i2c_read_sda(s)) {
        I2C_LOG("\nWARNING: %s: SDA=0\n", tag);
    }
    if (i2c_read_scl(s)) {
        I2C_LOG("\nWARNING: %s: SCL=0\n", tag);
    }
}

struct i2c_stop_state {
    void *next;
    struct i2c_port *port;
    uint32_t timeout; // for I2C_DELAY macro
};
void i2c_stop_init(struct i2c_stop_state *s, struct i2c_port *port) {
    s->port = port;
    s->next = 0;
}
sm_status_t i2c_stop_tick(struct i2c_stop_state *s) {
    SM_RESUME(s); I2C_STOP(s); SM_HALT(s);
}



struct i2c_send_byte_state {
    void *next;
    struct i2c_port *port;
    uint32_t timeout;
    int8_t clock;  // signed for negative sentinel
    uint8_t byte;
    uint8_t nack;
};
void i2c_send_byte_init(struct i2c_send_byte_state *s, struct i2c_port *port, uint8_t byte) {
    memset(s,0,sizeof(*s));
    s->port = port;
    s->byte = byte;
}



sm_status_t i2c_send_byte_tick(struct i2c_send_byte_state *s) {
    SM_RESUME(s);
    I2C_SEND_BYTE(s);
    /* Returns 0 on correct ack, 1 on nack. */
    SM_HALT_STATUS(s, s->nack);
}

struct i2c_recv_byte_state {
    void *next;
    struct i2c_port *port;
    uint32_t timeout;
    int8_t clock;
    uint8_t byte;
    uint8_t nack;
    uint8_t bitval;
};
void i2c_recv_byte_init(struct i2c_recv_byte_state *s, struct i2c_port *port, uint8_t nack) {
    memset(s,0,sizeof(*s));
    s->port = port;
    s->nack = nack;
}
sm_status_t i2c_recv_byte_tick(struct i2c_recv_byte_state *s) {
    SM_RESUME(s);
    I2C_RECV_BYTE(s);
    SM_HALT(s);
}


// Adapted from hw_i2c_*_state / _init / _tick in hw_i2c.h
// This is supposed to be a drop-in replacement.

struct i2c_control_state {
    // In this implementation, nack is the only thing that can go
    // wrong, which is most cases means that the device is not there.
    // Maybe also include here in what byte the error occurred?
    uint32_t sr;
};

struct i2c_transmit_state {
    struct i2c_control_state ctrl;
    void *next; // sm.h resume point
    const_slice_uint8_t hdr;
    const_slice_uint8_t data;
    uint8_t slave; // slave address
    int8_t i; // loop counter
    struct i2c_port *port;
    union {
        struct i2c_start_state      i2c_start;
        struct i2c_send_byte_state  i2c_send_byte;
    } sub;
};
struct i2c_receive_state {
    struct i2c_control_state ctrl;
    void *next; // sm.h resume point
    slice_uint8_t data;
    uint8_t slave; // slave address
    int8_t i; // loop counter
    struct i2c_port *port;
    union {
        struct i2c_start_state      i2c_start;
        struct i2c_send_byte_state  i2c_send_byte;
        struct i2c_recv_byte_state  i2c_recv_byte;
    } sub;
};

static inline void i2c_transmit_init(
    struct i2c_transmit_state *s,
    struct i2c_port *port,
    uint32_t slave,
    const uint8_t *hdr, uint32_t hdr_len,
    const uint8_t *data, uint32_t data_len) {

    memset(s,0,sizeof(*s));
    s->port = port;
    s->slave = slave;
    s->hdr.buf = hdr;
    s->hdr.len = hdr_len;
    s->data.buf = data;
    s->data.len = data_len;
}

#define I2C_SEND_SLICE(s, slice)                                        \
    if ((slice)->buf) {                                                 \
        for(s->i = 0; s->i < (slice)->len; s->i++) {                    \
            if (SM_SUB_CATCH(s, i2c_send_byte, s->port, (slice)->buf[s->i])) \
                goto nack;                                              \
        }                                                               \
    }

#define I2C_RECV_SLICE(s, slice)                                        \
    if ((slice)->buf) {                                                 \
        for(s->i = 0; s->i < (slice)->len; s->i++) {                    \
            int nack = s->i >= (slice)->len - 1;                        \
            SM_SUB_CATCH(s, i2c_recv_byte, s->port, nack);              \
            (slice)->buf[s->i] = s->sub.i2c_recv_byte.byte;             \
        }                                                               \
    }

static inline uint32_t i2c_transmit_tick(struct i2c_transmit_state *s) {
    SM_RESUME(s);
    SM_SUB_CATCH(s, i2c_start, s->port);
    if (SM_SUB_CATCH(s, i2c_send_byte, s->port, s->slave << 1 | I2C_W)) goto nack;
    I2C_SEND_SLICE(s,&s->hdr);
    I2C_SEND_SLICE(s,&s->data);
    s->ctrl.sr = 0;
    SM_HALT(s);
  nack:
    // In this implementation, nack is the only thing that can go
    // wrong, which is most cases means that the device is not there.
    // Maybe also include here in what byte the error occurred?
    s->ctrl.sr = 1;
    SM_HALT_STATUS(s, s->ctrl.sr);
}

static inline void i2c_receive_init(
    struct i2c_receive_state *s,
    struct i2c_port *port,
    uint32_t slave,
    uint8_t *data, uint32_t data_len) {

    memset(s,0,sizeof(*s));
    s->port = port;
    s->slave = slave;
    s->data.buf = data;
    s->data.len = data_len;
}

static inline uint32_t i2c_receive_tick(struct i2c_receive_state *s) {
    SM_RESUME(s);
    SM_SUB_CATCH(s, i2c_start, s->port);
    if (SM_SUB_CATCH(s, i2c_send_byte, s->port, s->slave << 1 | I2C_R)) goto nack;
    I2C_RECV_SLICE(s,&s->data);
    s->ctrl.sr = 0;
    SM_HALT(s);
  nack:
    s->ctrl.sr = 1;
    SM_HALT_STATUS(s, s->ctrl.sr);
}



typedef struct i2c_stop_state     i2c_stop_t;
typedef struct i2c_transmit_state i2c_transmit_t;
typedef struct i2c_receive_state  i2c_receive_t;
typedef struct i2c_control_state  i2c_control_t;


#endif
