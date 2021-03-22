/* Generic packet-based event loop.

   This is a radical simplification of the idea of an event loop, only
   supporting the {packet,4} protocol and only using input events.
   I.e. a stalled TCP write will block the entire scheduler.

   The main purpose of this code is to provide a simple framework for
   Linux emulators of event-driven bare-metal programs written on top
   of uc_tools csp and sm abstractions, where simplicity of
   implementation is much more important than dealing with TCP timing
   issues.

   The application registers a "push" method to receive the {packet,4}
   events.  This can then be used e.g. to drive a CSP scheduler.
*/

#include "packet_loop.h"

#include "tcp_tools.h"
#include "macros.h"
#include "uct_byteswap.h"
#include <poll.h>

void unregister_client(struct packet_loop_state *s, int i) {
    int fd = s->fds[i];
    close(fd);
    /* Move the last one to take the slot of the one moved. */
    s->fds[i] = s->fds[s->nfds-1];
    s->nfds--;
    if (s->config->notify) s->config->notify(s, fd, 0);
    //LOG("- fd=%d n=%d\n", fd, state.nfds);
}
void register_client(struct packet_loop_state *s, int fd) {
    ASSERT(s->nfds < s->max_nfds);
    s->fds[s->nfds++] = fd;
    if (s->config->notify) s->config->notify(s, fd, 1);
    //LOG("+ fd=%d n=%d\n", fd, state.nfds);
}
uint32_t read_client_bytes(struct packet_loop_state *s, int i, uint8_t *buf, uint32_t n) {
    int have = 0;
    int fd = s->fds[i];
    while (have < n) {
        int rv = read(fd, buf+have, n-have);
        if (rv <= 0) {
            if ((have == 0) && (rv == 0)) {
                // This is expected EOF.  Don't log anything.
            }
            else {
                const char *e = strerror(rv);
                LOG("WARNING: read_client_bytes: n=%d, rv=%d, %s: unregistering\n", n, rv, e);
            }
            unregister_client(s, i);
            return 0;
        }
        have += rv;
    }
    return n;
}
uint32_t read_client(struct packet_loop_state *s, int i_in) {
    const struct packet_loop_config *c = s->config;
    uint8_t buf0[c->packet_size];
    // Reserve some space for zero-copy downstream header wrapping.
    uint8_t *buf = &buf0[c->header_size];
    uint32_t n;
    if (4 != (n = read_client_bytes(s, i_in, buf, 4))) return 0;
    n = read_be(buf, 4);
    // LOG("n = 0x%x\n", n);
    if (n+4 > (c->packet_size - c->header_size)) {
        ERROR("buffer overflow 0x%x bytes\n", n+4);
    }
    if (n != read_client_bytes(s, i_in, buf+4, n)) return 0;
    //LOG("packet: %d\n", n);
    if (c->push) {
        c->push(s, s->fds[i_in], buf, n+4);
    }
    return n+4;
}

void packet_loop_start(struct packet_loop_state *s) {

    int server_fd = assert_tcp_listen(s->config->tcp_port);
    struct pollfd pfd[s->max_nfds];
    for(;;) {
      again:
        /* Set up client descriptors. */
        for (int i=0; i<s->nfds; i++) {
            pfd[i].events = POLLIN | POLLERR;
            pfd[i].fd = s->fds[i];
        }
        /* Set up server descriptor. */
        int i_server = s->nfds;
        pfd[i_server].events = POLLIN | POLLERR;
        pfd[i_server].fd = server_fd;

        /* Also monitor stdin for EOF. */
        int i_stdin = s->nfds+1;
        pfd[i_stdin].events = POLLIN | POLLERR;
        pfd[i_stdin].fd = 0;

        /* Wait */
        int rv;
        //LOG("waiting for %d clients\n", s->nfds);
        ASSERT_ERRNO(rv = poll(&pfd[0], s->nfds+2, -1));
        ASSERT(rv > 0);

        /* Handle */

        /* Note: if an error occurs and a client was removed, the
         * client table indices in the loop are no longer valid, so
         * restart the poll .*/

        for(int i=0; i<s->nfds; i++) {
            if(pfd[i].revents & POLLERR) {
                unregister_client(s, i);
                goto again;
            }
            if(pfd[i].revents & POLLIN) {
                uint32_t rv = read_client(s, i);
                if (!rv) {
                    goto again;
                }
                pfd[i].revents &= ~(POLLIN | POLLERR);
            }
        }
        if(pfd[i_server].revents & POLLIN) {
            struct sockaddr_in address_in;
            socklen_t addrlen = sizeof(address_in);
            int fd;
            ASSERT_ERRNO(
                (fd = accept(server_fd,
                             (struct sockaddr *)&address_in,
                             &addrlen)));
            register_client(s, fd);
            pfd[i_server].revents &= ~POLLIN;
        }
        if(pfd[i_stdin].revents & POLLIN) {
            /* stdin data is ignored.  this is here just to exit the
             * application if stdin closes. */
            uint8_t buf[1024];
            int rv;
            ASSERT((rv = read(0, buf, sizeof(buf))) > 0);
            LOG("ignoring %d bytes from stdin\n", rv);
            for (int i=0; i<rv; i++) LOG(" %02x",buf[i]); LOG("\n");
            pfd[i_stdin].revents &= ~POLLIN;
        }

        ASSERT(0 == pfd[i_server].revents);
        ASSERT(0 == pfd[i_stdin].revents);
    }
}

