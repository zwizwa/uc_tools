#if 0

// OLD GDBSTUB
#include "bl_hyministm32v.h"

int main(void) {
    hyministm32v_init();
    bootloader_init();
    // bootloader_blink_loop(LED);
    bootloader_loop();
    return 0;
}

#else

// NEW 3IF
// See also mod_bl_c8t6_a12b5_boot1.c
#include "bl_hyministm32v.h"
#include "mod_monitor.c"

BOOTLOADER_SERVICE(monitor_read,
                   monitor_write,
                   NULL)

int main(void) {
    hyministm32v_init();
    bootloader_init();
    monitor_init();
    bootloader_loop();
    return 0;
}

#endif
