/* {packet,4} echo server (pubsub, bus emulation) */
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
    LOG("unregister fd=%d, n=%d\n", fd, nb_clients);
}
void register_client(int fd) {
    client[nb_clients++].fd = fd;
    LOG("register fd=%d, n=%d\n", fd, nb_clients);
}
uint32_t read_client_bytes(int i, uint8_t *buf, uint32_t n) {
    int rv;
    int fd = client[i].fd;
    ASSERT((rv = read(fd, buf, n)) >= 0);
    if (rv == 0) {
        unregister_client(i);
        return 0;
    }
    else {
        //LOG("fd=%d, len=%d\n", fd, rv);
        return rv;
    }
}
void read_client(int i_in) {
    uint8_t buf[1024];
    uint32_t n;
    if (4 != (n = read_client_bytes(i_in, buf, 4))) return;
    n = read_be(buf, 4);
    if (n+4 > sizeof(buf)) {
        ERROR("buffer overflow %d bytes\n", n+4);
    }
    if (n != read_client_bytes(i_in, buf+4, n)) return;
    //LOG("packet: %d\n", n);
    for (int i=0; i<nb_clients; i++) {
        if (i != i_in) {
            ASSERT(n+4 == write(client[i].fd, buf, n+4));
        }
    }
}

int main(int argc, char **argv) {
    LOG("echo_server.c\n");
    ASSERT(argc == 2);
    int port = atoi(argv[1]);
    int server_fd = assert_tcp_listen(port);
    struct pollfd pfd[MAX_NB_CLIENTS];
    for(;;) {
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
                read_client(i);
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
