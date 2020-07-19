/* Library routines for linux file descriptor i/o */
#include "csp_fd.h"
#include "byteswap.h"
#include "packet_tags.h"
#include <unistd.h>

/* Implement the async buffer mechanism in csp.[ch] but for file
   descriptors, without the need to go through intermediate
   buffers. */

void csp_to_fd_init(struct csp_to_fd *s, int fd, uint16_t in_chan, uint16_t ext_chan) {
    memset(s, 0, sizeof(*s));
    s->fd = fd;
    s->in_chan = in_chan;
    s->ext_chan = ext_chan;
    s->task.resume = (csp_resume_f)csp_to_fd_resume;
}

void csp_to_fd_resume(struct csp_to_fd *s) {
    if (s->next) goto *s->next;
  again:
    CSP_EVT_SHARED(s, 0 /* s->evt[0] */, s->in_chan);
    CSP_SEL(s, 0 /*nb_send*/, 1 /*nb_recv*/);
    /* Shared data is valid up to the next suspension.  Cache it in
       local variables so C compiler can aid with lifetime
       warnings. */
    const uint8_t *buf = s->evt[0].msg_buf;
    uint16_t       len = s->evt[0].msg_len;
    LOG("csp_to_fd: %p %d\n", buf, len);
    /* Protocol header is currently hardcoded. */
    {
        uint8_t hdr[] = {
            U32_BE(len + 4),
            U16_BE(TAG_CSP),
            U16_BE(s->ext_chan), // shared channel/tag namspace
        };
        ASSERT(sizeof(hdr) == write(s->fd, hdr, sizeof(hdr)));
        ASSERT(len == write(s->fd, buf, len));
    }
    goto again;
}

/* For the receiver, we are going to assume that the application is
   built into an "async wrapper" that will take care of buffer
   allocation, and invokes a callback.  This means that csp_send() can
   be used. */





