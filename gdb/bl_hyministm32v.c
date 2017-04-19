#include "bl_hyministm32v.h"

int main(void) {
    hyministm32v_init();
    bootloader_init();
    // bootloader_blink_loop(LED);
    bootloader_loop();
    return 0;
}

