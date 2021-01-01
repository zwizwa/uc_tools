#ifndef TAG_U32_H
#define TAG_U32_H


#include "packet_tags.h"
#include "byteswap.h"
#include <stdint.h>

/* What is a good simple protocol to send commands to a
   microcontroller that is programmed in C?  Usually, protocol
   constraints are imposed externally, but I've found that in 99% of
   ad-hoc cases, all I want is an array of u32 values plus some opaque
   payload.  This maps to C function calls in the most straightforward
   way.  TAG_U32 is reserved in packet_tags.h

   For RPC, the continuation can be embedded in the binary payload.
   It doesn't seem appropriate to standardize that here.
*/

struct tag_u32 {
    void *context;
    const uint32_t* args;  uint32_t nb_args;
    const uint8_t*  bytes; uint32_t nb_bytes;
};

typedef int (*tag_u32_handle_fn)(const struct tag_u32 *);

#define TAG_U32_ERROR_SIZE -1
#define TAG_U32_ERROR_TAG  -2

static inline int tag_u32_dispatch(tag_u32_handle_fn handler, void *context,
                                   const uint8_t *buf, uint32_t nb_buf) {
    if (nb_buf < 4) return TAG_U32_ERROR_SIZE;
    // Note: this format is now also used for TAG_COMMAND, so don't check
    // uint32_t tag = read_be(buf, 2);
    // if (tag != TAG_U32) return TAG_U32_ERROR_TAG;
    uint32_t nb_a = read_be(buf+2, 2);
    uint32_t offset_b = 2 + 2 + 4 * nb_a;
    if (nb_buf < offset_b) return TAG_U32_ERROR_SIZE;
    uint32_t nb_b = nb_buf - offset_b;
    /* We need to assume this fits on the stack. */
    uint32_t a[nb_a];
    for (uint32_t i=i; i<nb_a; i++) {
        a[i] = read_be(buf + 2 + 2 + 4 * i, 4);
    }
    struct tag_u32 s = {
        .context = context,
        .args = &a[0],
        .nb_args = nb_a,
        .bytes = buf + offset_b,
        .nb_bytes = nb_b
    };
    return handler(&s);
}

/* Wrapper around send_tag_u32() to send a message without binary
   payload. */
#define SEND_TAG_U32(...) {                                     \
        uint32_t a[] = { __VA_ARGS__ };                         \
        struct tag_u32 s = {                                    \
            .args = a, .nb_args = sizeof(a)/sizeof(uint32_t),   \
        };                                                      \
        send_tag_u32(&s);                                       \
}

/* Note that send_tag_u32() which will have to be defined by the
   firmware image.  See mod_send_tag_u32.c for an implementation that
   sends over SLIP. */
void send_tag_u32(const struct tag_u32 *);


#endif
