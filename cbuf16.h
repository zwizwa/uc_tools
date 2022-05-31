#ifndef CBUF16_H
#define CBUF16_H

#include <stdint.h>
#include "macros.h"
#include "uc_tools_config.h"

/* Like cbuf.h, but based on 16 bit cell size instead of 8 bit. */
#include "cbuf.h"

struct cbuf16 {
    volatile uint32_t write;
    volatile uint32_t read;
    uint32_t size;
    volatile uint16_t *buf;
#ifdef CBUF_DEBUG
    volatile uint32_t watermark;
    volatile uint32_t overflow;
#endif
};

/* Typedefs and macros for ns_queue.h using the cbuf namespace prefix. */
typedef struct cbuf16 cbuf16_queue_t;
typedef uint16_t cbuf16_element_t;
typedef uint16_t cbuf16_oob_element_t;
static inline cbuf16_oob_element_t cbuf16_oob_element_none(void) { return CBUF_EAGAIN; }

/* Most of the functionality is inherited from the generic circular
   bufer in ns_cbuf.h */
#define NS(name) cbuf16##name
#define NS_CBUF_DEBUG               CBUF_DEBUG
#define NS_CBUF_DEBUG_INFO_OVERFLOW CBUF_DEBUG_INFO_OVERFLOW
#include "ns_cbuf.h"
#undef NS_CBUF_DEBUG
#undef NS_CBUF_DEBUG_INFO_OVERFLOW
#undef NS

/* Initialize with statically allocated buffer with _buf postfix. */
#define CBUF16_INIT(name) cbuf16_init(&name, &name##_buf[0], ARRAY_SIZE(name##_buf))

#define CBUF16_WRITE(buf, ...) \
    do { uint16_t msg[] = __VA_ARGS__; cbuf16_write(buf, msg, ARRAY_SIZE(msg)); } while(0)

#endif
