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

#include "mod_i2c_bitbang.c"
#include "mod_i2c_track_stm32f103.c"


struct i2c_tester_config {
    uint32_t us;
    uint8_t addr, stretch, d;
} i2c_tester_state = {
    .us = I2C_HALF_PERIOD_TICKS / 72,
    .stretch = 1,
    //.addr = 96,
    .addr = 0x54,  // eeprom first 256 bytes
    .d = 0,
};






#if 0
// No longer used?
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
#endif


/* 2. ASYNC MASTER */

// see mod_i2c_bitbang.c




/* 3. SLAVE */

// see mod_i2c_bitcrunch

struct cbuf i2c_tester_mbox; uint8_t i2c_tester_mbox_buf[64];


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



/* Blocking wrappers for mod_i2c_bitbang.c state machines. */
int send_byte(struct i2c_port *bus, int byte) {
    struct i2c_send_byte_state s;
    i2c_send_byte_init(&s, bus, byte);
    while(SM_WAITING == i2c_send_byte_tick(&s));
    return s.nack;
}
int recv_byte(struct i2c_port *bus, int nack) {
    struct i2c_recv_byte_state s;
    i2c_recv_byte_init(&s, bus, nack);
    while(SM_WAITING == i2c_recv_byte_tick(&s));
    return s.val;
}

void i2c_start(struct i2c_port *bus) {
    struct i2c_start_state s;
    i2c_start_init(&s, bus);
    while(SM_WAITING == i2c_start_tick(&s));
}

/* The rest is shared api. */
#include "mod_i2c_blocking_wrapper.c"






void info_ack(int nack) {
    if(!nack)
        info_putchar('a');
    else
        info_putchar('n');
}
int info_send_byte(struct i2c_port *bus, uint8_t b) {
    int nack = send_byte(bus, b);
    // infof(" %02x %d", b, nack);
    infof(" %02x%s", b, nack ? "(nack)" : "");
    // infof(" %02x", b);
    return nack;
}
int info_recv_byte(struct i2c_port *bus, int nack) {
#if I2C_DEBUG_SPACING
    I2C_NDELAY(s, 2); // spacing on scope
#endif
    int b = recv_byte(bus, nack);
    infof(" %02x", b);
    return b;
}

KEEP void eeprom_write(struct i2c_port *bus, uint8_t page, const uint8_t *buf, uintptr_t len) {
    i2c_start(bus);
    infof(" S");

    // return value 1 means nack
    if (info_send_byte(bus, i2c_tester_state.addr << 1 | I2C_W)) goto nack;
    if (info_send_byte(bus, page)) goto nack;
    for (uintptr_t i=0; i<len; i++) {
        if (info_send_byte(bus, buf[i])) goto nack;
    }
    goto stop;
  nack:
    infof("nack\n");
  stop:
    infof(" P");
    i2c_stop(bus);
    infof("\n");
    return;
}

void test_eeprom_read_1(struct i2c_port *bus) {
    i2c_start(bus);
    int nack = info_send_byte(bus, i2c_tester_state.addr << 1 | I2C_R);
    if (nack) goto stop;
    info_recv_byte(bus, 0);
  stop:
    i2c_stop(bus);

    //I2C_NDELAY(s, 3);
    //I2C_DEBLOCK(s);

}


KEEP intptr_t test_eeprom_read(struct i2c_port *bus,
                               int8_t offset, uint8_t *buf, uintptr_t len) {
    intptr_t rv = -1;

    i2c_start(bus);
    infof(" S");

    info_send_byte(bus, i2c_tester_state.addr << 1 | I2C_W);
    info_send_byte(bus, offset);

    i2c_stop(bus);

    i2c_start(bus);
    infof(" S");

    int nack = info_send_byte(bus, i2c_tester_state.addr << 1 | I2C_R);
    if (nack) {
        infof(" nack\n");
        goto stop;
    }

    //I2C_NDELAY(s, i2c_tester_state.d);

    intptr_t i;
    for (i=0; i<len; i++) {
        int nack = i >= len-1;
        buf[i] = info_recv_byte(bus, nack);
    }
    rv = len;

  stop:
    infof(" P");
    i2c_stop(bus);

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

void i2c_test_eeprom(struct i2c_port *bus) {
    if (0) {
        i2c_start(bus);
        send_byte(bus, 123);
        i2c_stop(bus);
    }

    if (0) {
        test_eeprom_read_1(bus);
    }
    if (1) {
        uint8_t buf[8] = "abcdefg";
        uint8_t page = 0;
        eeprom_write(bus, page, buf, sizeof(buf));
    }
    if (1) {
        uint8_t offset = 0;
        for(int j=0; j<3; j++) {
            uint8_t buf[16];
            for(int i=0; i<3; i++) {
                memset(buf, 0, sizeof(buf));
                test_eeprom_read(bus, offset, buf, sizeof(buf));
                //test_eeprom_read(buf, sizeof(buf));
                // hw_busywait_ms(100);
                // FIXME: eeprom gets confused.  deblock solves it.
                // i2c_deblock();
                // if (1) { info_ascii(buf, sizeof(buf)); }

                for(int c=0; c<sizeof(buf); c++) {
                    buf[c]++;
                }
                eeprom_write(bus, offset, buf, sizeof(buf));
            }
            offset += sizeof(buf);
        }
    }
}

KEEP void eeprom_write_read(struct i2c_port *bus) {

    i2c_start(bus);

    info_send_byte(bus, i2c_tester_state.addr << 1 | I2C_W);
    info_send_byte(bus, 1);

    i2c_start(bus);

    send_byte(bus, i2c_tester_state.addr << 1 | I2C_R);

    info_recv_byte(bus, 0);
    info_recv_byte(bus, 0);
    info_recv_byte(bus, 1);

    i2c_stop(bus);
}

/* Tiny command interpreter. To keep things simple, single letter
   commands are used. */
KEEP void i2c_tester_command_write(struct i2c_port *bus,
                                   const uint8_t *buf, uint32_t len) {
    void *s = NULL;
    int val;
    for(int i=0; i<len; i++) {
        int c = buf[i];
        infof("c:%02x --", c);
        switch(c) {
        case 'c':
            val = !i2c_read_scl(s);
            i2c_write_scl(s,val);
            infof("scl=%d", val);
            break;
        case 'd':
            val = !i2c_read_sda(s);
            i2c_write_sda(s,val);
            infof("sda=%d", val);
            break;
        case 'e':
            eeprom_write_read(bus);
            break;
        case 'w': {
            uint8_t buf[1] = {123};
            eeprom_write(bus, 0, buf, sizeof(buf));
            break;
        }
        case 'r': {
            uint8_t buf[1];
            test_eeprom_read(bus, 0, buf, sizeof(buf));
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
