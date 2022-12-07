 /* The idea here is to revive the old microcontroller script idea.
   A lot of code fits the following pattern:
   - Compile a C function to ARM code
   - Load it into uc RAM
   - Execute it
   - Go back to bootloader loop

   The main advantage of that is that the code on the microcontroller
   does not need to be "managed".  It just needs a boot loader.

   This one is built on top of the 3if monitor.

   The point is to keep it really simple, so the monitor and the
   script do not need to know about each other except for a way to
   pass parameters.  This is done using the monitor's data stack.

   The monitor doesn't need to know where the code is located.  Host
   code can assime it is at the start of RAM.  See x8ram linker
   script.

*/
#include <stdint.h>
struct run_3if {
    uint8_t *ds;
};

#include "hw_stm32f103.h"

// See lab_board.c
#define RELAY_A GPIOA,3
#define RELAY_B GPIOA,4
#define RELAY_C GPIOA,5
#define RELAY_D GPIOA,6

/* Goes into a separate section so linker script can place it into a
   predictable location, e.g. start of RAM segment. */
__attribute__((section(".run")))
int run(struct run_3if *s) {
    // Toggle relay = config gpio output, set, delay, unset
    hw_gpio_config(RELAY_A, HW_GPIO_CONFIG_OUTPUT);
    hw_gpio_config(RELAY_B, HW_GPIO_CONFIG_OUTPUT);

    hw_gpio_low(RELAY_A);
    hw_gpio_low(RELAY_B);
    hw_busywait_us(1000000);
    hw_gpio_high(RELAY_A);
    hw_gpio_high(RELAY_B);
    return 0;
}
