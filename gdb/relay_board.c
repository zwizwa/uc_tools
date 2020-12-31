/* Stand alone relay controller board.

   This serves as an example for how to write a very simple serial
   port application on top of the bootloader.
*/

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>


/* For ad-hoc protocols, it often makes sense to design it in a way
   that is easy to interpret on both ends.  This application is
   intended to be used together with erl_tools/src/gdbstub_hub.erl,
   which uses {packet,4} as Erlang input, and poses no constraint on
   the protocol we use for this side input.

   So we use single characters for port control, one for each relay:
   ABCD is 1, abcd is 0

   The return path sends an empty {packet,4} message for each command
   that has been received.

*/

KEEP void set_pin(int pin, int val) {
    hw_gpio_write(GPIOA,pin,val);
    hw_gpio_config(GPIOA,pin,HW_GPIO_CONFIG_OUTPUT);
}
uint32_t nb_commands = 0;
static void command_write(const uint8_t *buf, uint32_t len) {
    for(int i=0; i<len; i++) {
        int c = buf[i];
        switch(c) {
        case 'A': set_pin(3,1); break;
        case 'B': set_pin(4,1); break;
        case 'C': set_pin(5,1); break;
        case 'D': set_pin(6,1); break;
        case 'a': set_pin(3,0); break;
        case 'b': set_pin(4,0); break;
        case 'c': set_pin(5,0); break;
        case 'd': set_pin(6,0); break;
        }
        nb_commands++;
    }
}
static uint32_t command_read(uint8_t *buf, uint32_t len) {
    //return 0;

    uint32_t n = 0;
    while(nb_commands && ((n + 4) <= len)) {
        memset(&buf[n], 0, 4);  // empty {packet,4} message
        n += 4;
        nb_commands--;
    }
    return n;
}
const struct gdbstub_io command_io = {
    .read  = command_read,
    .write = command_write,
};



// BOILERPLATE


/* If bootloader sees a message that does not parse as GDB RSP, it
   passes it here so we can install a new i/o handler on the virtual
   serial port. */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    infof("Console on serial port.\n");
    *_service.io = (struct gdbstub_io *)(&command_io);
    (*_service.io)->write(buf, size);
}

void start(void) {
    /* Low level application init */
    hw_app_init();

    /* IO init */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);


}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Relay Board";
const char config_serial[]       CONFIG_DATA_SECTION = "2";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    .start           = start,
    .stop            = stop,
    .switch_protocol = switch_protocol,
};



