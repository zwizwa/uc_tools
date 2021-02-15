#ifndef MOD_I2C_TESTER
#define MOD_I2C_TESTER

/* I2C test firmware for Blue pill.

   This uses blue pill pins that can be muxed to I2C peripheral, even
   if we do just bit-bang now, to allow for peripheral use later.

   Internal pullups are currently not used.  Most setups in practice
   will use external pullups.

   FIXME: This still uses a flat namespace.  That's ok for now since
   it is only used on a dedicated board.


*/

/* 1. GENERIC */

#include "hw_stm32f103.h"
#include "cycle_counter.h"
#include "cbuf.h"
#include <stdint.h>
#include "sm.h"
#include "sm_def.h"

#ifndef I2C_PIN_SCL
#define I2C_PIN_SCL 8
#endif

#ifndef I2C_PIN_SDA
#define I2C_PIN_SDA 9
#endif

#ifndef I2C_GPIO
#define I2C_GPIO GPIOB
#endif

#define I2C_R 1
#define I2C_W 0

/* Half a clock period.  By default, run it slow.
   FIXME: It doesn't seem to work properly with 50,5 */
#ifndef I2C_TESTER_PERIOD_US
#define I2C_TESTER_PERIOD_US 500
#endif

struct cbuf i2c_tester_mbox; uint8_t i2c_tester_mbox_buf[64];


#define I2C_DEBUG_SPACING 0

struct i2c_tester_config {
    uint32_t us;
    uint8_t addr, stretch, d;
} i2c_tester_state = {
    .us = I2C_TESTER_PERIOD_US,
    .stretch = 1,
    //.addr = 96,
    .addr = 0x54,  // eeprom first 256 bytes
    .d = 0,
};

/* 1. NON BLOCKING CORE ROUTINES */

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



/* 2. POLLING MASTER ROUTINES.

   These are defined as macros, and parameterized by state, so we can
   plug in a task mechanism.
*/

/* Primitives: delay and condition poll.  These will "infect" all
   other code that is written in terms of them. */

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



void i2c_delay_busywait() {
    /* Wait for 1/2 I2C clock period.  At 10kHz this is 50us. */
    uint32_t ticks_per_us = 72;
    uint32_t timer = cycle_counter() + i2c_tester_state.us * ticks_per_us;
    for(;;) {
        int32_t left = timer - cycle_counter();
        if (left < 0) break;
    }
    // hw_busywait_us(i2c_tester_state.us);
}

// This is quite tricky with both lines involved in signaling.

#define I2C_WAIT_STRETCH(s) {                                           \
        /* PRE: SCL released */                                         \
        if (i2c_tester_state.stretch) I2C_WHILE(s, !i2c_scl_read());    \
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
            /*infof("\nWARNING: deblock clocks=%d\n", i);*/             \
            infof(" (%d)", s->clock);                                   \
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

void i2c_check_busy(const char *tag) {
    if (i2c_sda_read()) {
        infof("\nWARNING: %s: SDA=0\n", tag);
    }
    if (i2c_scl_read()) {
        infof("\nWARNING: %s: SCL=0\n", tag);
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


// FIXME: These should be the state machines

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
    SM_HALT(s);
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

#include "slice.h"

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
        struct i2c_stop_state       i2c_stop;
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
        struct i2c_stop_state       i2c_stop;
    } sub;
};

// FIXME: Change other API to handle stop as a state machine before
// merging.

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
    SM_SUB_CATCH0(s, i2c_stop);
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
    SM_SUB_CATCH0(s, i2c_stop);
    s->ctrl.sr = 0;
    SM_HALT(s);
  nack:
    s->ctrl.sr = 1;
    SM_HALT_STATUS(s, s->ctrl.sr);
}







/* 3. SLAVE

   This needs to respond immediately so is run from interrupt.  We use
   the EXTI mechanism, and implement the state machine in sm_cond.h
   conditional style, which means it will always always evaluate the
   state of the pins and will not rely on the information provided by
   the interrupt mechanism.  I.e. the state machine is essentially
   just polling mode, and the events are used to poll on _possible_
   state change.
*/

// 3.0 Abstract machine

#include "sm.h"
#define I2C_TRACK_SCL (1<<1)
#define I2C_TRACK_SDA (1<<0)

#define I2C_TRACK_WAIT_C1D1_IDLE(s)  SM_WAIT(s,   s->bus == 0b11)
#define I2C_TRACK_WAIT_C1D0_START(s) SM_WAIT(s,   s->bus == 0b10)
#define I2C_TRACK_WAIT_C1Dx_HI(s)    SM_WAIT(s,   s->bus &  0b10)
#define I2C_TRACK_WAIT_C0Dx_LO(s)    SM_WAIT(s, !(s->bus &  0b10))

struct i2c_track {
    void *next;
    uint8_t bus;    // current bus value
    uint8_t bus0;   // previous bus value (for STOP detection)
    uint8_t bit;    // burrent bit
    uint8_t sreg;   // shift register
};
struct i2c_track i2c_track;
void i2c_track_init(struct i2c_track *s) {
    memset(s,0,sizeof(*s));
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


// FIXME: This is not correct.  It's impossible to get this right
// while tired...

uint32_t i2c_track_tick(struct i2c_track *s) {
    SM_RESUME(s);

    I2C_TRACK_WAIT_C1D1_IDLE(s);

  stop:
    for(;;) {
        I2C_TRACK_WAIT_C1D0_START(s);
        I2C_TRACK_WAIT_C0Dx_LO(s); // write edge

        for(;;) {

            // data byte
            for(s->bit = 0; s->bit < 9; s->bit++) {
                I2C_TRACK_WAIT_C1Dx_HI(s); // read edge
                // sda must be stable until write edge
                s->sreg = (s->bus & 1) | (s->sreg << 1);

                // To distinguish between normal clock transition and
                // STOP, we wait for a change and inspect it.
                s->bus0 = s->bus;
                SM_WAIT(s, s->bus != s->bus0);

                // SDA 0->1 while SCL=1 is a STOP condition.  It
                // always indicates end of transmission, even in the
                // middle of a byte.
                if ((s->bus0 == 0b10) &&
                    (s->bus  == 0b11)) {
                    goto stop;
                }
                // SCL 1->0 with SDA=dontcare indicates a normal clock
                // edge, indicating a new bit is coming and the loop
                // can continue.
            }
        }
    }
}



// 3.1 TIM
// See mod_3level.c for inspiration
// Note that I2C slave is synchronouse, so timer is likely not necessary.

#if 0
#define TICKS_PER_US 100

const struct hw_delay hw_tim[] = {
//          rcc       irq            tim   psc
//-------------------------------------------------------
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, 72*TICKS_PER_US },
};
#define TIM 3
#define C_TIM hw_tim[TIM]
void HW_TIM_ISR(TIM)(void) {
    hw_delay_ack(C_TIM);
    // uint16 diff = 123;
    //hw_delay_arm(C_TIM, diff);
    //hw_delay_trigger(C_TIM);

}
void i2c_tester_tim_init(void) {
    hw_delay_init(C_TIM, 0xFFFF, 1 /*enable interrupt*/);
    hw_delay_arm(C_TIM, 1);
    hw_delay_trigger(C_TIM);
}
#endif



// 3.2 EXTI

// This extends hw_exti_ack() for multi-event setup.
// FIXME: Comment why both EMR nd PR are written.
static inline void hw_exti_do_ack(uint32_t pin) {
    *hw_bitband(&EXTI_EMR, pin) = 0; // rmw
    EXTI_PR = 1 << pin; // w
}
static uint32_t isr_count;
void exti9_5_isr(void) {
    /* We just need to be woken up on change, so it's ok if this
       handles a simultaneous change on 8 and 9.  */
    hw_exti_do_ack(8);
    hw_exti_do_ack(9);
    isr_count++;
    i2c_track.bus = (i2c_scl_read() << 1) | i2c_sda_read();
    i2c_track_tick(&i2c_track);



}
// This extends hw_exti_arm() for multi-event setup.
static inline void hw_exti_do_arm(uint32_t gpio, uint32_t pin, uint32_t trigger) {
    uint32_t exti = 1 << pin;
    exti_select_source(exti, gpio);
    exti_set_trigger(exti, trigger);
    exti_enable_request(exti);
}
void i2c_tester_exti_init(void) {
    /* The config is special because we handle multiple pins in a
       single interrupt, so do it manually here.  For inspiration, see
       hw_stm32103.h */
    rcc_periph_clock_enable(RCC_GPIOB);
    rcc_periph_clock_enable(RCC_AFIO);
    nvic_enable_irq(NVIC_EXTI9_5_IRQ);
    hw_exti_do_arm(GPIOB, 8, EXTI_TRIGGER_BOTH);
    hw_exti_do_arm(GPIOB, 9, EXTI_TRIGGER_BOTH);
}

void i2c_tester_poll(void) {
    while(cbuf_bytes(&i2c_tester_mbox)) {
        uint8_t token = cbuf_get(&i2c_tester_mbox);
        (void)token;
        //infof("%d\n", token);
    }
#if 1
    static uint32_t count_handled;
    static uint32_t timer;
    MS_PERIODIC(timer, 1000) {
        uint32_t count = isr_count;
        int32_t new_count = count - count_handled;
        if (new_count) {
            infof("%d %d\n", new_count, count);
        }
        count_handled = count;
    }
#endif
}
instance_status_t i2c_tester_init(instance_init_t *ctx) {
    CBUF_INIT(i2c_tester_mbox);
    rcc_periph_clock_enable(RCC_GPIOA | RCC_GPIOB | RCC_AFIO);
    enable_cycle_counter();
    i2c_tester_exti_init();
    _service.add(i2c_tester_poll);
    return 0;
}
DEF_INSTANCE(i2c_tester);




/* 4. USER INTERFACE */


int map_i2c_tester(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
    };
    return HANDLE_TAG_U32_MAP(req, map);
}


/* 5. TEST CODE */



/* Blocking wrppers. */
int send_byte(int byte) {
    struct i2c_send_byte_state s;
    i2c_send_byte_init(&s, byte);
    while(SM_WAITING == i2c_send_byte_tick(&s));
    return s.nack;
}
int recv_byte(int nack) {
    struct i2c_recv_byte_state s;
    i2c_recv_byte_init(&s, nack);
    while(SM_WAITING == i2c_recv_byte_tick(&s));
    return s.val;
}
void i2c_start(void) {
    struct i2c_start_state s;
    i2c_start_init(&s);
    while(SM_WAITING == i2c_start_tick(&s));
}
void i2c_stop(void) {
    struct i2c_stop_state s;
    i2c_stop_init(&s);
    while(SM_WAITING == i2c_stop_tick(&s));
}






void info_ack(int nack) {
    if(!nack)
        info_putchar('a');
    else
        info_putchar('n');
}
int info_send_byte(uint8_t b) {
    int nack = send_byte(b);
    // infof(" %02x %d", b, nack);
    infof(" %02x%s", b, nack ? "(nack)" : "");
    // infof(" %02x", b);
    return nack;
}
int info_recv_byte(int nack) {
#if I2C_DEBUG_SPACING
    I2C_NDELAY(s, 2); // spacing on scope
#endif
    int b = recv_byte(nack);
    infof(" %02x", b);
    return b;
}

KEEP void eeprom_write(uint8_t page, const uint8_t *buf, uintptr_t len) {
    infof(" S");
    i2c_start();

    // return value 1 means nack
    if (info_send_byte(i2c_tester_state.addr << 1 | I2C_W)) goto nack;
    if (info_send_byte(page)) goto nack;
    for (uintptr_t i=0; i<len; i++) {
        if (info_send_byte(buf[i])) goto nack;
    }
    goto stop;
  nack:
    infof("nack\n");
  stop:
    infof(" P");
    i2c_stop();
    infof("\n");
    return;
}

void eeprom_read_1(void) {
    i2c_start();
    int nack = info_send_byte(i2c_tester_state.addr << 1 | I2C_R);
    if (nack) goto stop;
    info_recv_byte(0);
  stop:
    i2c_stop();

    //I2C_NDELAY(s, 3);
    //I2C_DEBLOCK(s);

}


KEEP intptr_t eeprom_read(uint8_t offset, uint8_t *buf, uintptr_t len) {
    intptr_t rv = -1;

    infof(" S");
    i2c_start();

    info_send_byte(i2c_tester_state.addr << 1 | I2C_W);
    info_send_byte(offset);

    infof(" S");
    i2c_start();

    int nack = info_send_byte(i2c_tester_state.addr << 1 | I2C_R);
    if (nack) {
        infof(" nack\n");
        goto stop;
    }

    //I2C_NDELAY(s, i2c_tester_state.d);

    intptr_t i;
    for (i=0; i<len; i++) {
        int nack = i >= len-1;
        buf[i] = info_recv_byte(nack);
    }
    rv = len;

  stop:
    infof(" P");
    i2c_stop();

    infof("\n");

    return rv;
}

void info_ascii(uint8_t *buf, uint32_t len) {
    for(int c=0; c<sizeof(buf); c++) {
        if ((buf[c] >= ' ') && (buf[c] < 127)) {
            info_putchar(buf[c]);
        }
        else {
            info_putchar('.');
        }
    }
    info_putchar('\n');
}

void i2c_test_eeprom(void) {
    if (0) {
        i2c_start();
        send_byte(123);
        i2c_stop();
    }

    if (0) {
        eeprom_read_1();
    }
    if (1) {
        uint8_t buf[8] = "abcdefg";
        uint8_t page = 0;
        eeprom_write(page, buf, sizeof(buf));
    }
    if (1) {
        uint8_t offset = 0;
        for(int j=0; j<3; j++) {
            uint8_t buf[16];
            for(int i=0; i<3; i++) {
                memset(buf, 0, sizeof(buf));
                eeprom_read(offset, buf, sizeof(buf));
                //eeprom_read(buf, sizeof(buf));
                // hw_busywait_ms(100);
                // FIXME: eeprom gets confused.  deblock solves it.
                // i2c_deblock();
                // if (1) { info_ascii(buf, sizeof(buf)); }

                for(int c=0; c<sizeof(buf); c++) {
                    buf[c]++;
                }
                eeprom_write(offset, buf, sizeof(buf));
            }
            offset += sizeof(buf);
        }
    }
}

KEEP void eeprom_write_read() {

    i2c_start();

    info_send_byte(i2c_tester_state.addr << 1 | I2C_W);
    info_send_byte(1);

    i2c_start();

    send_byte(i2c_tester_state.addr << 1 | I2C_R);

    info_recv_byte(0);
    info_recv_byte(0);
    info_recv_byte(1);

    i2c_stop();
}

/* Tiny command interpreter. To keep things simple, single letter
   commands are used. */
KEEP void i2c_tester_command_write(const uint8_t *buf, uint32_t len) {
    int val;
    for(int i=0; i<len; i++) {
        int c = buf[i];
        infof("c:%02x --", c);
        switch(c) {
        case 'c':
            val = !i2c_scl_read();
            i2c_write_scl(val);
            infof("scl=%d", val);
            break;
        case 'd':
            val = !i2c_sda_read();
            i2c_write_sda(val);
            infof("sda=%d", val);
            break;
        case 'e':
            eeprom_write_read();
            break;
        case 'w': {
            uint8_t buf[1] = {123};
            eeprom_write(0, buf, sizeof(buf));
            break;
        }
        case 'r': {
            uint8_t buf[1];
            eeprom_read(0, buf, sizeof(buf));
            break;
        }
        case '[':
            if (i2c_tester_state.us>1) i2c_tester_state.us--;
            infof("us=%d", i2c_tester_state.us);
            break;
        case ']':
            i2c_tester_state.us++;
            infof("us=%d", i2c_tester_state.us);
            break;
        case '+':
            i2c_tester_state.d++;
            infof("d=%d", i2c_tester_state.d);
            break;
        case '-':
            if(i2c_tester_state.d) i2c_tester_state.d--;
            infof("d=%d", i2c_tester_state.d);
            break;
        case 's':
            i2c_tester_state.stretch=!i2c_tester_state.stretch;
            infof("stretch=%d", i2c_tester_state.stretch);
            break;
        }
        infof("\r\n");
    }
}



#endif
