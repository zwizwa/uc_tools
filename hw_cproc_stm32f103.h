#ifndef HW_CPROC_STM32F013_H
#define HW_CPROC_STM32F013_H

/* See comments here. */
#include "cproc.h"
#include "hw_stm32f103.h"

typedef struct { w out; } gpin_state;
typedef struct { } gpin_input;
typedef struct { uint32_t port, pin; } gpin_config;
DEF_PROC(gpin, c, s, i) {
    s->out = hw_gpio_read(c->port, c->pin);
} __attribute__((always_inline))

typedef struct { } gpout_state;
typedef struct { w in; } gpout_input;
typedef struct { uint32_t port, pin; } gpout_config;
DEF_PROC(gpout, c, s, i) {
    hw_gpio_write(c->port, c->pin, i->in);
} __attribute__((always_inline))

#endif
