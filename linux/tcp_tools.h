#ifndef TCP_TOOLS_H
#define TCP_TOOLS_H

#include "macros.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>

static inline int assert_tcp_listen(int port) {
    int sockfd = 0;
    ASSERT(port > 0);
    ASSERT(port < 0x10000);
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
    LOG("listening on 0.0.0.0:%d\n", port);
    return sockfd;
}


static inline int assert_tcp_connect(const char *host, int port) {
    struct hostent *hp;
    ASSERT(port > 0);
    ASSERT(port < 0x10000);
    ASSERT(hp = gethostbyname(host));
    struct sockaddr_in address_in = {
        .sin_port = htons((uint16_t)port),
        .sin_family = AF_INET
    };
    memcpy((char *)&address_in.sin_addr,
           (char *)hp->h_addr_list[0], hp->h_length);
    int sockfd = 0;
    ASSERT_ERRNO(sockfd = socket(AF_INET, SOCK_STREAM, 0));
    int intarg = -1;
    ASSERT_ERRNO(setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, &intarg, sizeof(intarg)));
    socklen_t addrlen = sizeof(address_in);
    ASSERT_ERRNO(connect(sockfd, (struct sockaddr *) &address_in, addrlen));
    LOG("connected to %s:%d\n", host, port);
    return sockfd;
}


#endif

