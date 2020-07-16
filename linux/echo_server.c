/* {packet,4} echo server (pubsub, bus emulation) */
#define _GNU_SOURCE
#include "macros.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
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
    LOG("unregister %d (%d)\n", fd, nb_clients);
}
void register_client(int fd) {
    client[nb_clients++].fd = fd;
    LOG("register %d (%d)\n", fd, nb_clients);
}
void read_client(int i) {
    uint8_t buf[1024];
    int rv;
    int fd = client[i].fd;
    ASSERT((rv = read(fd, buf, sizeof(buf))) >= 0);
    if (rv == 0) {
        unregister_client(i);
    }
    else {
        LOG("fd=%d, len=%d\n", fd, rv);
    }
}


int server(int port) {
    int sockfd = 0;
    ASSERT(port > 0);
    ASSERT(port < 0x10000);
    // create socket
    ASSERT_ERRNO(sockfd = socket(AF_INET, SOCK_STREAM, 0));
    int intarg = -1;
    ASSERT_ERRNO(setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, &intarg, sizeof(intarg)));
    ASSERT_ERRNO(setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &intarg, sizeof(intarg)));
    struct sockaddr_in address_in = {
        .sin_port = htons((uint16_t)port),
        .sin_family = AF_INET
    };
    socklen_t addrlen = sizeof(address_in);
    ASSERT_ERRNO(bind(sockfd, (struct sockaddr *) &address_in, addrlen));
    int backlog = 5;
    ASSERT_ERRNO(listen(sockfd, backlog));
    LOG("listening on port %d\n", port);
    return sockfd;
}

int main(int argc, char **argv) {
    LOG("echo_server.c\n");
    ASSERT(argc == 2);
    int port = atoi(argv[1]);
    int server_fd = server(port);
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
        LOG("waiting for %d clients\n", nb_clients);
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
