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
#include "byteswap.h"
#include <poll.h>


#define MAX_NB_CLIENTS 10
static int client_fd[MAX_NB_CLIENTS];
static struct packet_loop_state state = {.fds = &client_fd[0] };

void unregister_client(const struct packet_loop_config *config, int i) {
    int fd = client_fd[i];
    close(fd);
    /* Move the last one to take the slot of the one moved. */
    client_fd[i] = client_fd[state.nfds-1];
    state.nfds--;
    if (config->notify) config->notify(&state, fd, 0);
    //LOG("- fd=%d n=%d\n", fd, state.nfds);
}
void register_client(const struct packet_loop_config *config, int fd) {
    client_fd[state.nfds++] = fd;
    if (config->notify) config->notify(&state, fd, 1);
    //LOG("+ fd=%d n=%d\n", fd, state.nfds);
}
uint32_t read_client_bytes(const struct packet_loop_config *config, int i, uint8_t *buf, uint32_t n) {
    int have = 0;
    int fd = client_fd[i];
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
            unregister_client(config, i);
            return 0;
        }
        have += rv;
    }
    return n;
}
uint32_t read_client(const struct packet_loop_config *c, int i_in) {
    uint8_t buf0[c->packet_size];
    // Reserve some space for zero-copy downstream header wrapping.
    uint8_t *buf = &buf0[c->header_size];
    uint32_t n;
    if (4 != (n = read_client_bytes(c, i_in, buf, 4))) return 0;
    n = read_be(buf, 4);
    // LOG("n = 0x%x\n", n);
    if (n+4 > (c->packet_size - c->header_size)) {
        ERROR("buffer overflow 0x%x bytes\n", n+4);
    }
    if (n != read_client_bytes(c, i_in, buf+4, n)) return 0;
    //LOG("packet: %d\n", n);
    if (c->push) {
        c->push(&state, client_fd[i_in], buf, n+4);
    }
    return n+4;
}

void packet_loop_start(const struct packet_loop_config *config) {
    int server_fd = assert_tcp_listen(config->tcp_port);
    struct pollfd pfd[MAX_NB_CLIENTS];
    for(;;) {
      again:
        /* Set up descriptors. */
        for (int i=0; i<state.nfds; i++) {
            pfd[i].events = POLLIN | POLLERR;
            pfd[i].fd = client_fd[i];
        }
        int i_server = state.nfds;
        pfd[i_server].events = POLLIN | POLLERR;
        pfd[i_server].fd = server_fd;

        /* Wait */
        int rv;
        //LOG("waiting for %d clients\n", state.nfds);
        ASSERT_ERRNO(rv = poll(&pfd[0], state.nfds+1, -1));
        ASSERT(rv > 0);

        /* Handle */

        /* Note: if an error occurs and a client was removed, the
         * client table indices in the loop are no longer valid, so
         * restart the poll .*/

        for(int i=0; i<state.nfds; i++) {
            if(pfd[i].revents & POLLERR) {
                unregister_client(config, i);
                goto again;
            }
            if(pfd[i].revents & POLLIN) {
                uint32_t rv = read_client(config, i);
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
            register_client(config, fd);
            pfd[i_server].revents &= ~POLLIN;
        }
        ASSERT(0 == pfd[i_server].revents);
    }
}

