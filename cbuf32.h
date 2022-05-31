#ifndef CBUF32_H
#define CBUF32_H

#include <stdint.h>
#include "macros.h"
#include "uc_tools_config.h"

/* Like cbuf.h, but based on 32 bit cell size instead of 8 bit. */
#include "cbuf.h"

struct cbuf32 {
    volatile uint32_t write;
    volatile uint32_t read;
    uint32_t size;
    volatile uint32_t *buf;
#ifdef CBUF_DEBUG
    volatile uint32_t watermark;
    volatile uint32_t overflow;
#endif
};

/* Typedefs and macros for ns_queue.h using the cbuf namespace prefix. */
typedef struct cbuf32 cbuf32_queue_t;
typedef uint32_t cbuf32_element_t;
typedef uint32_t cbuf32_oob_element_t;
static inline cbuf32_oob_element_t cbuf32_oob_element_none(void) { return CBUF_EAGAIN; }

/* Most of the functionality is inherited from the generic circular
   bufer in ns_cbuf.h */
#define NS(name) cbuf32##name
#define NS_CBUF_DEBUG               CBUF_DEBUG
#define NS_CBUF_DEBUG_INFO_OVERFLOW CBUF_DEBUG_INFO_OVERFLOW
#include "ns_cbuf.h"
#undef NS_CBUF_DEBUG
#undef NS_CBUF_DEBUG_INFO_OVERFLOW
#undef NS

/* Initialize with statically allocated buffer with _buf postfix. */
#define CBUF32_INIT(name) cbuf32_init(&name, &name##_buf[0], ARRAY_SIZE(name##_buf))

#define CBUF32_WRITE(buf, ...) \
    do { uint32_t msg[] = __VA_ARGS__; cbuf32_write(buf, msg, ARRAY_SIZE(msg)); } while(0)

#endif
