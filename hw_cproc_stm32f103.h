#ifndef HW_CPROC_STM32F013_H
#define HW_CPROC_STM32F013_H

/* See comments here. */
#include "cproc.h"
#include "hw_stm32f103.h"

#define for_gpin_state(m) m(w,out)
#define for_gpin_input(m)
#define for_gpin_config(m) m(uint32_t, port) m(uint32_t, pin)
#define for_gpin_param(m)
DEF_PROC(gpin, s, c, p, i) {
    s->out = hw_gpio_read(c->port, c->pin);
} __attribute__((always_inline))

#define for_gpout_state(m)
#define for_gpout_input(m) m(w, in)
#define for_gpout_config(m) m(uint32_t, port) m(uint32_t, pin)
#define for_gpout_param(m)
DEF_PROC(gpout, s, c, p, i) {
    hw_gpio_write(c->port, c->pin, i->in);
} __attribute__((always_inline))

#endif
