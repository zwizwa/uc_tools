/* lwIP arch for the i686 kernel plugins, see plugin.h */
#ifndef LWIP_ARCH_CC_H
#define LWIP_ARCH_CC_H

#include <stdint.h>
#include <stddef.h>
#include <sys/types.h>  // newlib ssize_t
#define SSIZE_MAX 0x7fffffff

int kernel_infof(const char *fmt, ...);
extern volatile uint32_t kernel_ms;

#define LWIP_NO_INTTYPES_H 1
#define LWIP_NO_CTYPE_H    1
#define LWIP_PROVIDE_ERRNO 1
#define X8_F  "02x"
#define U16_F "d"
#define S16_F "d"
#define X16_F "x"
#define U32_F "d"
#define S32_F "d"
#define X32_F "x"
#define SZT_F "d"

#define LWIP_PLATFORM_DIAG(x)   do { kernel_infof x; } while (0)
#define LWIP_PLATFORM_ASSERT(x) do { kernel_infof("lwip assert: %s\n", x); } while (0)
#define LWIP_RAND()             ((u32_t)kernel_ms * 1103515245u + 12345u)

#endif
