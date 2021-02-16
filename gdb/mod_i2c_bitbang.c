#ifndef MOD_I2C_BITBANG
#define MOD_I2C_BITBANG

/* I2C Master bitbang implementation. */

#include "sm.h"
#include "sm_def.h"
#include "slice.h"


/* 1. CONFIGURATION */

#ifndef I2C_PIN_SCL
#define I2C_PIN_SCL 8
#endif

#ifndef I2C_PIN_SDA
#define I2C_PIN_SDA 9
#endif

#ifndef I2C_GPIO
#define I2C_GPIO GPIOB
#endif

#ifndef I2C_STRETCH
#define I2C_STRETCH 1
#endif

#define I2C_DEBUG_SPACING 0

//#define I2C_LOG LOG
#define I2C_LOG(...)

/* Half a clock period.  By default, run it slow.
   FIXME: It doesn't seem to work properly with 50,5 */
#ifndef I2C_TESTER_PERIOD_US
#define I2C_TESTER_PERIOD_US 500
#endif


/* 2. HAL */

// FIXME: Just a stub to make emulator compile. Move this to the proper place
#ifdef EMU

static inline void i2c_write_sda(int bitval) {}
static inline void i2c_write_scl(int bitval) {}
static inline int i2c_sda_read(void) { return 1; }
static inline int i2c_scl_read(void) { return 1; }

#else

#include "hw_stm32f103.h"
#include "cycle_counter.h"


/* This is still tied to stm32f103, but there's no reason why it can't
   be made portable. */

/* On STM32, pull direction is set by the output data register ODR,
   which we access through hw_gpio_write().  There are only 2 states.
   These high()/low() functions provide glitch-free switching between
   them.  Order of operations is important.  Annotated with Hoare
   logic conditions. */
static inline void i2c_pin_high(uint32_t gpio, uint32_t pin) {
    // PRE: if pulling high, no state changes.
    // PRE: if asserting low, state changes annotated below
    hw_gpio_config(gpio, pin, HW_GPIO_CONFIG_INPUT_PULL);
    // pulling low
    hw_gpio_write(gpio, pin, 1);
    // POST: pulling high
}

static inline void i2c_pin_low(uint32_t gpio, uint32_t pin) {
    // PRE: if asserting low, no state changes
    // PRE: if apulling high, state changes annotated below
    hw_gpio_write(gpio, pin, 0);
    // pulling low
    hw_gpio_config(gpio, pin, HW_GPIO_CONFIG_OUTPUT);
    // POST: asserting low
}
static inline void i2c_pin_set(uint32_t gpio, uint32_t pin, uint32_t val) {
    if (val)
        i2c_pin_high(gpio, pin);
    else
        i2c_pin_low(gpio, pin);
}

static inline void i2c_write_sda(int bitval) {
    i2c_pin_set(I2C_GPIO, I2C_PIN_SDA, bitval);
}
static inline void i2c_write_scl(int bitval) {
    i2c_pin_set(I2C_GPIO, I2C_PIN_SCL, bitval);
}
static inline int i2c_sda_read(void) {
    return hw_gpio_read(I2C_GPIO, I2C_PIN_SDA);
}
static inline int i2c_scl_read(void) {
    return hw_gpio_read(I2C_GPIO, I2C_PIN_SCL);
}

#endif



#define I2C_R 1
#define I2C_W 0

/* 3. ASYNC MASTER

   A note on design: the async generators / protothreads / state
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

/* Synchronization primitives: time delay to implement the clock, and
   i/o condition poll to implement clock stretching by slave.  These
   will "infect" all other code that is written in terms of them. */

/* This can be used to move the blocking loop into the inner loop, for
   testing. */
// #define I2C_WHILE(s,cond) while(cond)
#define I2C_WHILE(s,cond) SM_WHILE(s,cond)

#define I2C_DELAY(s) {                                                  \
        s->timeout = cycle_counter_future_time(I2C_TESTER_PERIOD_US * 72); \
        SM_WAIT(s, cycle_counter_expired(s->timeout));                  \
    }
#define I2C_NDELAY(s,n_init) \
    {int n = n_init; while(n--) I2C_DELAY(s); }


#define I2C_WAIT_STRETCH(s) {                                           \
        /* PRE: SCL released */                                         \
        if (I2C_STRETCH) I2C_WHILE(s, !i2c_scl_read());                 \
    }
#define I2C_WRITE_SCL_1(s) {                    \
        i2c_write_scl(1);                       \
        I2C_WAIT_STRETCH(s);                    \
    }

#define I2C_DEBLOCK(s) {                                                \
        for(s->clock=0; s->clock<16; s->clock++) {                      \
            int sda1 = i2c_sda_read();                                  \
            int scl1 = i2c_scl_read();                                  \
            if (sda1 && scl1) break;                                    \
            i2c_write_scl(0); I2C_DELAY(s);                             \
            i2c_write_scl(1); I2C_DELAY(s);  /* no stretch */           \
        }                                                               \
        if (s->clock) {                                                 \
            /*I2C_LOG("\nWARNING: deblock clocks=%d\n", i);*/             \
            I2C_LOG(" (%d)", s->clock);                                   \
        }                                                               \
    }

#define I2C_START(s) {                                                  \
        /* PRE: */                                                      \
        /* - idle:   SDA=1,SCL=1 */                                     \
        /* - repeat: SDA=?,SCL=0 */                                     \
        /* For repeated start: bring lines high without causing a stop. */ \
        i2c_write_sda(1);   I2C_DELAY(s);                               \
        I2C_WRITE_SCL_1(s); I2C_DELAY(s);                               \
        /* START transition = SDA 1->0 while SCL=1 */                   \
        i2c_write_sda(0);   I2C_DELAY(s);                               \
        /* Bring clock line low for first bit */                        \
        i2c_write_scl(0);   I2C_DELAY(s);                               \
        /* POST: SDA=0, SCL=0 */                                        \
}

/* Deblock is most useful right before start, so inline it into the
   start state machine. */
struct i2c_start_state {
    void *next;
    uint32_t timeout; // for I2C_DELAY macro
    int8_t clock; // for I2C_DEBLOCK
};
void i2c_start_init(struct i2c_start_state *s) {
    s->next = 0;
}
sm_status_t i2c_start_tick(struct i2c_start_state *s) {
    SM_RESUME(s);
    I2C_DEBLOCK(s);
    I2C_START(s);
    SM_HALT(s);
}

#define I2C_SEND_BIT(s, val) {  \
        /* PRE: SDA=?, SCL=0 */                                         \
        i2c_write_sda(val); I2C_DELAY(s);  /* set + propagate */        \
        I2C_WRITE_SCL_1(s); I2C_DELAY(s);  /* set + wait read */        \
        i2c_write_scl(0);                                               \
        /* POST: SDA=x, SCL=0 */                                        \
}

#define I2C_RECV_BIT(s, lval) {                                         \
        /* PRE: SDA=?, SCL=0 */                                         \
        i2c_write_sda(1);   I2C_DELAY(s);  /* release, allow slave write */ \
        I2C_WRITE_SCL_1(s); I2C_DELAY(s);  /* slave write propagation */ \
        lval = i2c_sda_read();                                          \
        i2c_write_scl(0);                                               \
        /* POST: SDA=1, SCL=0 */                                        \
    }

void i2c_info_busy(const char *tag) {
    if (i2c_sda_read()) {
        I2C_LOG("\nWARNING: %s: SDA=0\n", tag);
    }
    if (i2c_scl_read()) {
        I2C_LOG("\nWARNING: %s: SCL=0\n", tag);
    }
}

struct i2c_stop_state {
    void *next;
    uint32_t timeout; // for I2C_DELAY macro
};
void i2c_stop_init(struct i2c_stop_state *s) {
    s->next = 0;
}
#define I2C_STOP(s) {                           \
        /* PRE: SDA=?, SCL=0 */                 \
        i2c_write_sda(0);   I2C_DELAY(s);               \
        I2C_WRITE_SCL_1(s); I2C_DELAY(s);               \
        /* STOP transition = SDA 0->1 while SCL=1 */    \
        i2c_write_sda(1);  I2C_DELAY(s);                 \
        /* POST: SDA=1, SCL=1  (unless held by slave) */ \
}
sm_status_t i2c_stop_tick(struct i2c_stop_state *s) {
    SM_RESUME(s); I2C_STOP(s); SM_HALT(s);
}



struct i2c_send_byte_state {
    void *next;
    uint32_t timeout;
    int8_t clock;  // signed for negative sentinel
    uint8_t byte;
    uint8_t nack;
};
void i2c_send_byte_init(struct i2c_send_byte_state *s, uint8_t byte) {
    memset(s,0,sizeof(*s));
    s->byte = byte;
}
sm_status_t i2c_send_byte_tick(struct i2c_send_byte_state *s) {
    SM_RESUME(s);
    for (s->clock=7; s->clock>=0; s->clock--) {
        I2C_SEND_BIT(s, (s->byte >> s->clock)&1);
    }
#if I2C_DEBUG_SPACING
    I2C_NDELAY(s, 2);
#endif
    I2C_RECV_BIT(s, s->nack);
    /* Returns 0 on correct ack, 1 on nack. */
    SM_HALT_STATUS(s, s->nack);
}

struct i2c_recv_byte_state {
    void *next;
    uint32_t timeout;
    int8_t clock;
    uint8_t val;
    uint8_t nack;
    uint8_t bitval;
};
void i2c_recv_byte_init(struct i2c_recv_byte_state *s, uint8_t nack) {
    memset(s,0,sizeof(*s));
    s->nack = nack;
}
sm_status_t i2c_recv_byte_tick(struct i2c_recv_byte_state *s) {
    SM_RESUME(s);
    for (s->clock=7; s->clock>=0; s->clock--) {
        I2C_RECV_BIT(s, s->bitval);
        s->val |= s->bitval << s->clock;
    }
#if I2C_DEBUG_SPACING
    I2C_NDELAY(s, 2);
#endif
    I2C_SEND_BIT(s, s->nack);
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
    union {
        struct i2c_start_state      i2c_start;
        struct i2c_send_byte_state  i2c_send_byte;
        struct i2c_recv_byte_state  i2c_recv_byte;
    } sub;
};

static inline void i2c_transmit_init(
    struct i2c_transmit_state *s,
    uint32_t slave,
    const uint8_t *hdr, uint32_t hdr_len,
    const uint8_t *data, uint32_t data_len) {

    memset(s,0,sizeof(*s));
    s->slave = slave;
    s->hdr.buf = hdr;
    s->hdr.len = hdr_len;
    s->data.buf = data;
    s->data.len = data_len;
}

#define I2C_SEND_SLICE(s, slice)                                        \
    if ((slice)->buf) {                                                 \
        for(s->i = 0; s->i < (slice)->len; s->i++) {                    \
            if (SM_SUB_CATCH(s, i2c_send_byte, (slice)->buf[s->i]))     \
                goto nack;                                              \
        }                                                               \
    }

#define I2C_RECV_SLICE(s, slice)                                        \
    if ((slice)->buf) {                                                 \
        for(s->i = 0; s->i < (slice)->len; s->i++) {                    \
            int nack = s->i >= (slice)->len - 1;                        \
            SM_SUB_CATCH(s, i2c_recv_byte, nack);                       \
            (slice)->buf[s->i] = s->sub.i2c_recv_byte.val;              \
        }                                                               \
    }

static inline uint32_t i2c_transmit_tick(struct i2c_transmit_state *s) {
    SM_RESUME(s);
    SM_SUB_CATCH0(s, i2c_start);
    if (SM_SUB_CATCH(s, i2c_send_byte, s->slave << 1 | I2C_W)) goto nack;
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
    uint32_t slave,
    uint8_t *data, uint32_t data_len) {

    memset(s,0,sizeof(*s));
    s->slave = slave;
    s->data.buf = data;
    s->data.len = data_len;
}

static inline uint32_t i2c_receive_tick(struct i2c_receive_state *s) {
    SM_RESUME(s);
    SM_SUB_CATCH0(s, i2c_start);
    if (SM_SUB_CATCH(s, i2c_send_byte, s->slave << 1 | I2C_R)) goto nack;
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
