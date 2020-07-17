/* Packet echo server, complementing packet_bridge.c

   This only supports the {packet,4} protocol with TCP clients.
   Use packet_bridge to interface other packet protocols.
*/

#include "packet_bus.h"

#include "tcp_tools.h"
#include "macros.h"
#include "byteswap.h"
#include <poll.h>


#define MAX_NB_CLIENTS 10
struct client {
    int fd;
};
struct client client[MAX_NB_CLIENTS] = {};
uint32_t nb_clients = 0;

void unregister_client(int i) {
    int fd = client[i].fd;
    close(fd);
    /* Move the last one to take the slot of the one moved. */
    client[i] = client[nb_clients-1];
    nb_clients--;
    LOG("- fd=%d n=%d\n", fd, nb_clients);
}
void register_client(int fd) {
    client[nb_clients++].fd = fd;
    LOG("+ fd=%d n=%d\n", fd, nb_clients);
}
uint32_t read_client_bytes(int i, uint8_t *buf, uint32_t n) {
    int fd = client[i].fd;
    int rv = read(fd, buf, n);
    if (rv <= 0) {
        unregister_client(i);
        return 0;
    }
    else {
        //LOG("fd=%d, len=%d\n", fd, rv);
        return rv;
    }
}
uint32_t read_client(const struct packet_bus_config *c, int i_in) {
    uint8_t buf[1024];
    uint32_t n;
    if (4 != (n = read_client_bytes(i_in, buf, 4))) return 0;
    n = read_be(buf, 4);
    if (n+4 > sizeof(buf)) {
        ERROR("buffer overflow %d bytes\n", n+4);
    }
    if (n != read_client_bytes(i_in, buf+4, n)) return 0;
    //LOG("packet: %d\n", n);
    if (c->sniff) {
        c->sniff(client[i_in].fd, buf, n+4);
    }

    for (int i=0; i<nb_clients; i++) {
        if (i != i_in) {
            ASSERT(n+4 == write(client[i].fd, buf, n+4));
        }
    }
    return n+4;
}

void packet_bus_start(const struct packet_bus_config *config) {
    int server_fd = assert_tcp_listen(config->tcp_port);
    struct pollfd pfd[MAX_NB_CLIENTS];
    for(;;) {
      again:
        /* Set up descriptors. */
        for (int i=0; i<nb_clients; i++) {
            pfd[i].events = POLLIN | POLLERR;
            pfd[i].fd = client[i].fd;
        }
        int i_server = nb_clients;
        pfd[i_server].events = POLLIN | POLLERR;
        pfd[i_server].fd = server_fd;

        /* Wait */
        int rv;
        //LOG("waiting for %d clients\n", nb_clients);
        ASSERT_ERRNO(rv = poll(&pfd[0], nb_clients+1, -1));
        ASSERT(rv > 0);

        /* Handle */
        for(int i=0; i<nb_clients; i++) {
            if(pfd[i].revents & POLLIN) {
                uint32_t rv = read_client(config, i);
                if (!rv) {
                    /* Error occured and client table changed, so
                     * indices are no longer valid.  Restart the
                     * poll .*/
                    goto again;
                }
                pfd[i].revents &= ~(POLLIN | POLLERR);
            }
            ASSERT(0 == pfd[i].revents);
        }
        if(pfd[i_server].revents & POLLIN) {
            struct sockaddr_in address_in;
            socklen_t addrlen = sizeof(address_in);
            int fd;
            ASSERT_ERRNO(
                (fd = accept(server_fd,
                             (struct sockaddr *)&address_in,
                             &addrlen)));
            register_client(fd);
            pfd[i_server].revents &= ~POLLIN;
        }
        ASSERT(0 == pfd[i_server].revents);
    }
}

