/* See uc_tools/stm32f103/script_3if.c

   Run code from SRAM using tether_bl
*/
#include <stdint.h>
struct run_3if {
    uint8_t *ds;
};
void    push (struct run_3if *s, uint8_t val) { *(s->ds)++ = val; }
uint8_t pop  (struct run_3if *s) { return *--(s->ds); }

#include "hw_stm32f103.h"

// See lab_board.c
#define RELAY_A GPIOA,3
#define RELAY_B GPIOA,4
#define RELAY_C GPIOA,5
#define RELAY_D GPIOA,6

#define SET(rel, state) \
    hw_gpio_config(rel, HW_GPIO_CONFIG_OUTPUT); \
    hw_gpio_write(rel, state); \


/* Goes into a separate section so linker script can place it into a
   predictable location, e.g. start of RAM segment. */
__attribute__((section(".run")))
int run(struct run_3if *s) {
    uint8_t relays = pop(s);
    uint8_t state = !!pop(s);
    if (relays & (1 << 0)) { SET(RELAY_A, state); }
    if (relays & (1 << 1)) { SET(RELAY_B, state); }
    if (relays & (1 << 2)) { SET(RELAY_C, state); }
    if (relays & (1 << 3)) { SET(RELAY_D, state); }
    return 0;
}
