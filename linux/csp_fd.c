/* Library routines for linux file descriptor i/o */
#include "csp_fd.h"
#include "uct_byteswap.h"
#include "packet_tags.h"
#include <unistd.h>

/* Asynchronous message send to external file descriptor.  This is NOT
   rendez-vous.  See remarks below.  Note that the dual (receive) will
   typically be implemented in an event loop as a callback, so it is
   not implemented with direct access to a file descriptor.  Also note
   that this implies the file descriptor does not block.  This is ok
   for the purpose it was designed for, but is not ok in general!  See
   remarks in packet_loop.c */

void csp_to_fd_init(struct csp_to_fd *s, int fd, uint16_t in_chan, uint16_t ext_chan) {
    memset(s, 0, sizeof(*s));
    s->fd = fd;
    s->in_chan = in_chan;
    s->ext_chan = ext_chan;
    s->task.resume = (csp_resume_f)csp_to_fd_resume;
}
csp_status_t csp_to_fd_resume(struct csp_to_fd *s) {
    if (s->next) goto *s->next;
  again:
    CSP_EVT_SHARED(s, 0 /* s->evt[0] */, s->in_chan);
    CSP_SEL(&(s->task), s, 0 /*nb_send*/, 1 /*nb_recv*/);
    /* Shared data is valid up to the next suspension.  Cache it in
       local variables so C compiler can aid with lifetime
       warnings. */
    const uint8_t *buf = s->evt[0].msg.u8;
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


/* There is a fundamental issue that prevents implementing network
   communication as rendez-vous: simultaneity does not exist across a
   network link due to time delays.  Send and receive across a network
   link are not the same event.

   In practice this is not a big issue.  It is possible to create
   other synchronization mechanisms such as RPC (two-step send
   request, recieve response) that work for synchronous as wel as
   asynchronous channels, but the symmetry of rendez-vous is lost.
*/
