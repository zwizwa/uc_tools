/* Generic lab module.
   See e.g. lab1.c */

#include <stdint.h>
#include "base.h"

#include "gdbstub_api.h"
#include <string.h>


/* Protocol-wise we don't really need anything special, so use
   slipstub_buffered to do the basics (SLIP framing, PING, GDB, INFO)
   and use tag_u32 for app-specific commands. */
#include "slipstub.h"
struct slipstub slipstub;
struct slipstub_buffers slipstub_buffers;

#include "tag_u32.h"

#include "instance.h"
#include "reset_device.h"
#include "cycle_counter.h"

#include "mod_console.c"

/* Provided by main file. */
int handle_tag_u32(struct tag_u32 *);
void setup(void);
void loop(void);

/* lab_board.erl now supports commands like this:

   hy1 ! {leds,[10,0,0]}.

   where the atom is the command, and the rest are the numbers passed
   on the stack before executing.

   this should probably be embedded in tha TAG_U32 discovery protocol

*/
int handle_command(struct tag_u32 *s) {
    if (s->nb_bytes > 0) {
        char command[s->nb_bytes+1];
        memcpy(command, s->bytes, s->nb_bytes);
        command[s->nb_bytes] = 0;
        FOR_COMMAND(c) {
            if (!strcmp(command, (*c)->name)) {
                for(uint32_t i=0; i<s->nb_args; i++) {
                    command_stack_push(s->args[i]);
                }
                (*c)->run();
                return 0;
            }
        }
    }
    return -1;
}
void handle_tag(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    // infof("tag %d\n", tag);
    switch(tag) {
    case TAG_COMMAND: {
        int rv = tag_u32_dispatch(
            handle_command,
            send_reply_tag_u32,
            NULL, p->buf, p->count);
        if (rv) { infof("handle_command returned %d\n", rv); }
        break;
    }
    case TAG_U32: {
        /* name ! {send_u32, [101, 1000000000, 1,2,3]}. */
        int rv = tag_u32_dispatch(
            handle_tag_u32,
            send_reply_tag_u32,
            NULL, p->buf, p->count);
        if (rv) {
            infof("mod_lab: handle_tag_u32 returned %d\n", rv);
        }
        break;
    }
    case TAG_RESET: {
        reset_device();
        break;
    }
    default:
        infof("unknown tag 0x%x\n", tag);
    }
}
#include "mod_send_tag_u32_slip.c"


#define MS_PERIODIC(var, ms) \
    CYCLE_COUNTER_PERIODIC(var, ((ms) * 72000))


/* STARTUP */

extern const struct instance app;
void start(void) {
    hw_app_init();
    /* FIXME: This assumes it's GPIOA */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_GPIOB | RCC_AFIO);

    /* This is fairly new. Won't display properly for old bootloaders. */
    //infof("stack_lo %x\n", _service.stack_lo);
    //infof("stack_hi %x\n", _service.stack_hi);

    enable_cycle_counter();

    /* Use framwork for handling incoming USB SLIP commands. */
    slipstub_init(handle_tag);

    /* It's just so much better developing with a watchdog enabled... */
    iwdg_set_period_ms(1000);
    iwdg_start();
    _service.add(iwdg_reset);

    instance_need_top(32, &app);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

#ifndef VERSION
#define VERSION "current"
#endif

#ifndef PRODUCT
#error need PRODUCT
#endif

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = PRODUCT;
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,lab_board,slip}";

extern const char config_version[];

#include "info_buf.h"

extern struct info_buf_hdr info_buf;

extern uint8_t _firmware_endx;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .stop            = stop,
    .switch_protocol = slipstub_switch_protocol,
    .monitor         = console_monitor,
    .info_buf        = &info_buf,
    .flash_start     = (const void*)&config,
    .flash_endx      = (void*)&_firmware_endx

};

