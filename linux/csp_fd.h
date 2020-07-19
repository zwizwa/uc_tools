#ifndef CSP_FD_H
#define CSP_FD_H

/* CSP file descriptor interface.  Note that we implement this only
   for {packet,4} protocol.  Generalization is for later, as
   needed. */

#include "csp.h"

struct csp_to_fd {
    struct csp_task task;
    struct csp_evt evt[1];
    int fd;
    void *next;
    uint16_t in_chan;
    uint16_t ext_chan;
};

void csp_to_fd_init(struct csp_to_fd *s, int fd, uint16_t in_chan, uint16_t ext_chan);
void csp_to_fd_resume(struct csp_to_fd *s);

#endif
