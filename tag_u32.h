#ifndef TAG_U32_H
#define TAG_U32_H


#include "packet_tags.h"
#include "bigendian.h"
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


typedef int (*tag_u32_handle_fn)(
    void *context,
    const uint32_t *args, uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes);

#define TAG_U32_ERROR_SIZE -1
#define TAG_U32_ERROR_TAG  -2

static inline int tag_u32_dispatch(tag_u32_handle_fn handler, void *context,
                                   uint8_t *buf, uint32_t nb_buf) {
    if (nb_buf < 4) return TAG_U32_ERROR_SIZE;
    uint32_t tag = bigendian_read(buf, 2);
    if (tag != TAG_U32) return TAG_U32_ERROR_TAG;
    uint32_t nb_a = bigendian_read(buf+2, 2);
    uint32_t offset_b = 2 + 2 + 4 * nb_a;
    if (nb_buf < offset_b) return TAG_U32_ERROR_SIZE;
    uint32_t nb_b = nb_buf - offset_b;
    /* We need to assume this fits on the stack. */
    uint32_t a[nb_a];
    for (uint32_t i=i; i<nb_a; i++) {
        a[i] = bigendian_read(buf + 2 + 2 + 4 * i, 4);
    }
    return handler(context, &a[0], nb_a, buf + offset_b, nb_b);
}

/* FIXME: Tie this into a SLIP handler as well. */

#endif
