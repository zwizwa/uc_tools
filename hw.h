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

#elif defined(EMU)
/* Do not include any platform-specific code.  It is assumed that the
   ad-hoc emulator build will fill in the HAL through other means. */

#elif defined(__linux__)

/* E.g. for emulator tests.
   Assume that stubs will be provided. */

#else
#error Unsupported hardware
#endif

#endif

