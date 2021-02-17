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
#include "mod_i2c_bitcrunch.c"





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

/* The rest is shared api. */
#include "mod_i2c_blocking_wrapper.c"






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
    i2c_start();
    infof(" S");

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

void test_eeprom_read_1(void) {
    i2c_start();
    int nack = info_send_byte(i2c_tester_state.addr << 1 | I2C_R);
    if (nack) goto stop;
    info_recv_byte(0);
  stop:
    i2c_stop();

    //I2C_NDELAY(s, 3);
    //I2C_DEBLOCK(s);

}


KEEP intptr_t test_eeprom_read(uint8_t offset, uint8_t *buf, uintptr_t len) {
    intptr_t rv = -1;

    i2c_start();
    infof(" S");

    info_send_byte(i2c_tester_state.addr << 1 | I2C_W);
    info_send_byte(offset);

    i2c_stop();

    i2c_start();
    infof(" S");

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
        test_eeprom_read_1();
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
                test_eeprom_read(offset, buf, sizeof(buf));
                //test_eeprom_read(buf, sizeof(buf));
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
            test_eeprom_read(0, buf, sizeof(buf));
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
