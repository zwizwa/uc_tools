#ifndef MOD_SM_TAGS
#define MOD_SM_TAGS

/* A protothread for handling command stream I/O.
   Focus is on TAG_U32, to be able to support large payloads.

   Part of the receiver is implemented.  No transmitter.

   This turns out to be quite clumsy for the obvious reason that it
   doesn't compose well, and their special composition mechanism
   infects everything.  It's probably ok to have reasonable complexity
   when only the larger payloads are streamed.  Those are going to be
   rare and can be special-cased.

   The same problem happens for the transmitter: the tx buffer
   callback would need to delegate to writer machines, and there will
   need to be a queue of these + their memory needs to be managed.

   None of this is simple. The tipping point where OS stacks start to
   pay of isn't very far.

   So when does it make sense to use this approach?
   I can't really say.


*/

#include <stdint.h>
#include "sm.h"
#include "byteswap.h"
#include "packet_tags.h"
#include "tag_u32.h"

/* Only needs to hold integers. */
#define SM_TAGS_BUF_SIZE 4

#define SM_TAGS_MAX_TAGS 16
struct sm_tags {
    void *next;
    uint8_t buf[SM_TAGS_BUF_SIZE];
    uint8_t *write;
    uint32_t left;
    uint32_t packet_size;
    union {
        struct {
            uint32_t tags[SM_TAGS_MAX_TAGS];
            uint8_t i,j;
            uint8_t nb_from, nb_args;
        } t;
    };
};

/* The machine only blocks on a read, and is only woken up if the read
   succeeds.  This makes it a pure switching coroutine that can just
   suspend and resume without additional conditions. */

#define SM_TAGS_DROP(s,n) { s->write = 0; s->left = n; SM_SUSPEND(s); }
#define SM_TAGS_READ(s,n) {                            \
        s->write = s->buf;                             \
        s->left = n;                                   \
        SM_SUSPEND(s);                                 \
}

#define SM_TAGS_UINT(s,n) ({ SM_TAGS_READ(s,n); read_be(s->buf, n); })

/* The above sometimes gives gcc warning that the anonymous variable
   might be uninitialized.  That's strange...  Try again later?  For
   now, work around it by setting variable. */
#define SM_TAGS_UINT_INTO(s,n,v) { s->write = s->buf; s->left = n; SM_SUSPEND(s); v = read_be(s->buf, n); }

struct sm_tags sm_tags;

void sm_tags_dispatch_tag_u32(struct sm_tags *s, uint8_t *buf) {
    uint32_t nf = s->t.nb_from;
    uint32_t na = s->t.nb_args;
    uint32_t nb = s->packet_size - 4 - 4 * (nf + na);
    struct tag_u32 t = {
        .nb_from = nf,
        .nb_args = na,
        .from = s->t.tags,
        .args = s->t.tags + nf,
        .nb_bytes = nb
    };
    LOG("dispatch nf=%d na=%d nb=%d\n", nf, na, t.nb_bytes);

    /* If buf is true the payload has been buffered.

       If it is too large it won't be buffered, and the dispatcher
       needs to handle this by providing a sink for us to write to.

       Additionally, reply packets and all other packets going in the
       other direction share the same fate: sources need to be
       provided to read from.

       FIXME: I'm starting to think this really isn't worth it...  It
       makes much more sense to use a little memory for stacks.
    */

}


sm_status_t sm_tags_tick(struct sm_tags *s) {
    SM_RESUME(s);

  next_packet:
    s->packet_size = SM_TAGS_UINT(s, 4);
    // LOG("size %d\n", s->packet_size);
    if (s->packet_size < 2) {
        /* Too small to contain a tag, just drop it. */
        SM_TAGS_DROP(s, s->packet_size);
    }
    else {
        uint16_t tag = SM_TAGS_UINT(s, 2);
        // LOG("tag %d\n", tag);
        switch(tag) {
        case TAG_U32:
            // LOG("TAG_U32\n");
            s->t.nb_from = SM_TAGS_UINT(s, 1);
            s->t.nb_args = SM_TAGS_UINT(s, 1);
            for(s->t.i = 0; s->t.i < s->t.nb_from + s->t.nb_args; s->t.i++) {
                // s->t.tags[s->t.i] = SM_TAGS_UINT(s,4);
                SM_TAGS_UINT_INTO(s, 4, s->t.tags[s->t.i]);
            }
            /* Dispatch doesn't block, so put it in a function.  If we
               don't buffer the payload here, dispatch is required to
               provide us with a sink for the payload. */
            {
                uint32_t nf = s->t.nb_from;
                uint32_t na = s->t.nb_args;
                uint32_t nb = s->packet_size - 4 - 4 * (nf + na);
                if (nb <= SM_TAGS_BUF_SIZE) {
                    SM_TAGS_READ(s, nb);
                    sm_tags_dispatch_tag_u32(s, s->buf);
                }
                else {
                    sm_tags_dispatch_tag_u32(s, 0);
                    /* FIXME: The dispatcher will have provided us
                       with a sink (a continuation).  We need to feed
                       that with data. */
                }
            }
            break;
        default:
            LOG("drop\n");
            SM_TAGS_DROP(s, s->packet_size - 2);
            break;
        }
    }
    goto next_packet;
}


void sm_tags_write(struct sm_tags *s, const uint8_t *buf, uintptr_t size) {
    while (size) {
        uint32_t chunk = size < s->left ? size : s->left;
        if (s->write) {
            memcpy(s->write, buf, chunk);
            s->write += chunk;
        }
        s->left -= chunk;
        if (!s->left) {
            sm_tags_tick(s);
        }
        size -= chunk;
        buf += chunk;
    }
}

#endif
