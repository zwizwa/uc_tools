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

#define SCL GPIOB,8
#define SDA GPIOB,9

#define I2C_R 1
#define I2C_W 0

/* By default, run it slow. */
#ifndef I2C_TESTER_PERIOD_US
#define I2C_TESTER_PERIOD_US 50
#endif

struct cbuf i2c_tester_mbox; uint8_t i2c_tester_mbox_buf[64];



struct i2c_tester_config {
    uint32_t us;
    uint8_t addr, stretch, d;
} i2c_tester_state = {
    .us = I2C_TESTER_PERIOD_US,
    .stretch = 1,
    .addr = 96,
    .d = 0,
};


/* On STM32, pull direction is set by the output data register ODR,
   which we access through hw_gpio_write().  There are only 2 states.
   These high()/low() functions provide glitch-free switching between
   them.  Order of operations is important.  Annotated with Hoare
   logic conditions. */
static inline void pin_high(uint32_t gpio, uint32_t pin) {
    // PRE: if pulling high, no state changes.
    // PRE: if asserting low, state changes annotated below
    hw_gpio_config(gpio, pin, HW_GPIO_CONFIG_INPUT_PULL);
    // pulling low
    hw_gpio_write(gpio, pin, 1);
    // POST: pulling high
}

static inline void pin_low(uint32_t gpio, uint32_t pin) {
    // PRE: if asserting low, no state changes
    // PRE: if apulling high, state changes annotated below
    hw_gpio_write(gpio, pin, 0);
    // pulling low
    hw_gpio_config(gpio, pin, HW_GPIO_CONFIG_OUTPUT);
    // POST: asserting low
}
static inline void pin_set(uint32_t gpio, uint32_t pin, uint32_t val) {
    if (val)
        pin_high(gpio, pin);
    else
        pin_low(gpio, pin);
}


void delay() {
    /* Wait for 1/2 I2C clock period.  At 10kHz this is 50us. */
    hw_busywait_us(i2c_tester_state.us);
}
void ndelay(int n) {
    while(n--) delay();
}
static inline void sda(int bitval) { pin_set(SDA, bitval); }
static inline void scl(int bitval) { pin_set(SCL, bitval); }
static inline int sda_read(void) { return hw_gpio_read(SDA); }
static inline int scl_read(void) { return hw_gpio_read(SCL); }


// This is quite tricky with both lines involved in signaling.

void wait_stretch(void) {
    // PRE: SCL released
    if (i2c_tester_state.stretch) while (!scl_read());
}

void scl_1(void) {
    scl(1);
    wait_stretch();
}

/* 2. MASTER

   Busyloop bitbanged for convenience. */

void send_start(void) {
    // PRE:
    // - idle:   SDA=1,SCL=1
    // - repeat: SDA=?,SCL=0
    // For repeated start: bring lines high without causing a stop.
    sda(1);  delay();
    scl_1(); delay();
    // START transition = SDA 1->0 while SCL=1
    sda(0);  delay();
    // Bring clock line low for first bit
    scl(0);  delay();
    // POST: SDA=0, SCL=0
}
void send_bit(int val) {
    // PRE: SDA=1, SCL=0
    sda(val); delay();  // set + propagate
    scl_1();  delay();  // set + wait read
    scl(0);
    // POST: SDA=x, SCL=0
}
int recv_bit(void) {
    // PRE: SDA=?, SCL=0
    int val;
    sda(1);   delay();  // release, allow slave write
    scl_1();  delay();  // slave write propagation
    val = sda_read();
    scl(0);
    return val;
    // POST: SDA=1, SCL=0
}
void send_stop(void) {
    // PRE: SDA=?, SCL=0
    sda(0);  delay();
    scl_1(); delay();
    // STOP transition = SDA 0->1 while SCL=1
    sda(1);  delay();
    // POST: SDA=1, SCL=1
}


int send_byte(int byte) {
    for (int bit=7; bit>=0; bit--) { send_bit((byte >> bit)&1); }
    return recv_bit();
}
void send_word(int word) {
    send_byte(word >> 8);
    send_byte(word);
}
int recv_byte(int ack) {
    int val = 0;
    for (int bit=7; bit>=0; bit--) { val |= (recv_bit() << bit); }
    send_bit(ack); // ack
    return val;
}
int recv_word(int ack) {
    int val = (recv_byte(0) << 8);
    val |= recv_byte(ack);
    return val;
}




void info_ack(int ack) {
    if(!ack)
        info_putchar('a');
    else
        info_putchar('n');
}
void info_send_byte(uint8_t b) {
    int ack = send_byte(b);
    infof(" %02x %d", b, ack);
}
int info_recv_byte(int ack) {
    int b = recv_byte(ack);
    infof(" %02x", b);
    return b;
}

KEEP void eeprom_write() {
    send_start();

    info_send_byte(i2c_tester_state.addr << 1 | I2C_W);
    info_send_byte(1);

    send_stop();
}

KEEP void eeprom_read() {
    send_start();

    info_send_byte(i2c_tester_state.addr << 1 | I2C_R);

    ndelay(i2c_tester_state.d);

    info_recv_byte(1);

    send_stop();
}


KEEP void eeprom_write_read() {

    send_start();

    info_send_byte(i2c_tester_state.addr << 1 | I2C_W);
    info_send_byte(1);

    send_start();

    send_byte(i2c_tester_state.addr << 1 | I2C_R);

    info_recv_byte(0);
    info_recv_byte(0);
    info_recv_byte(1);

    send_stop();
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
    i2c_track.bus = (scl_read() << 1) | sda_read();
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

/* Tiny command interpreter. To keep things simple, single letter
   commands are used. */
KEEP void i2c_tester_command_write(const uint8_t *buf, uint32_t len) {
    int val;
    for(int i=0; i<len; i++) {
        int c = buf[i];
        infof("c:%02x --", c);
        switch(c) {
        case 'c':
            val = !scl_read();
            scl(val);
            infof("scl=%d", val);
            break;
        case 'd':
            val = !sda_read();
            sda(val);
            infof("sda=%d", val);
            break;
        case 'e':
            eeprom_write_read();
            break;
        case 'w':
            eeprom_write();
            break;
        case 'r':
            eeprom_read();
            break;
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


int map_i2c_tester(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
    };
    return HANDLE_TAG_U32_MAP(req, map);
}


#endif
