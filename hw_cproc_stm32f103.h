#ifndef HW_CPROC_STM32F013_H
#define HW_CPROC_STM32F013_H

/* See comments here. */
#include "cproc.h"
#include "hw_stm32f103.h"

/* Parameterizable processor. */
typedef struct { w out; } gpio_state;
typedef struct { } gpio_input;
typedef struct { uint32_t port, pin; } gpio_config;
DEF_PROC(gpio, c, s, i) {
    s->out = hw_gpio_read(c->port, c->pin);
} __attribute__((always_inline))

#endif
