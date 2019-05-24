#ifndef HW_STM32F407_H
#define HW_STM32F407_H

/* This code is based for the larger part on libopencm3 code.
   E.g. hw_usart_enable() is a copy of libopencm3's usart_enable(), etc.. */

/* Parameterization of generic STM32F code */
#define HW_LOOPS_PER_US 12

//#warning F407

#include "hw_stm32f.h"
// FIXME: 407 versions of this
#include <libopencm3/stm32/f4/rcc.h>
#include <libopencm3/stm32/f4/timer.h>
#include <libopencm3/stm32/f4/gpio.h>
#include <libopencm3/stm32/f4/usart.h>
#include <libopencm3/stm32/f4/spi.h>


#endif // HW_STM32F407_H


