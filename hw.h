#ifndef HW_H
#define HW_H

/* The hardware support in uc_tools is based on libopencm3, providing
 * some faster inline routines for specific hardware. I.e. partially
 * unabstracted.
 *
 * FIXME: This should probably contain a default implementation that
 * does just plain libopencm3. */

#if defined(STM32F1)
#include "hw_stm32f103.h"

#elif defined(STM32F4)
#include "hw_stm32f407.h"

#else

#error Unsupported hardware
#endif

#endif

