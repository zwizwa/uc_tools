#include "bl_hyministm32v.h"

/* NOTE: Do not autostart burried devices without hardware override!
   E.g. use RS485 break condition to stay in bootloader, or make sure
   JTAG pins are accessible to restore to a working state. */
int main(void) {
    hyministm32v_init();
    bootloader_init();
    if (!flash_null(_config.start)) _config.start();
    bootloader_blink_loop(LED);
    // bootloader_loop();
    return 0;
}

