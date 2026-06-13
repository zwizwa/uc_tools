/* The point of this thing is to provide an upload and command tool
   for i686/kernel.c based on idempotent commands.  For memory write,
   kernel just responds to packets, sends one ack per packet.  We keep
   going until we got an ack per block back.  This avoids tftp-style
   round-trip delays and keeps the implementation on the target very
   simple. */


/* This uses a generalization of the 3if protocol
   - One UDP message can contain a stream of commands
   - Operation can be asynchronous
   - Separate transport header with sequence number, which can be reused after rx ack
   - Should probably limit to idempotent commands only
   - 1-1 relation between UDP packets, but can contain multiple commands
   - Don't rely on 3if machine state between packets (drops)
*/

#include "macros.h"

#include "tcp_tools.h"


struct tether_udp {
    struct sockaddr_in peer;
    int fd;
};


void tether_udp_send(struct tether_udp *s, const uint8_t *buf, uint32_t len) {
    //log_addr(&p->peer);
    int flags = 0;
    int wlen;
    ASSERT_ERRNO(
        wlen = sendto(s->fd, buf, len, flags,
                      (struct sockaddr*)&s->peer,
                      sizeof(s->peer)));
    (void)wlen;
}
void tether_udp_send_str(struct tether_udp *s, const char *str) {
    tether_udp_send(s, (const uint8_t*)str, strlen(str));
}

void tether_udp_init(struct tether_udp *s, const char *host, uint16_t port) {
    memset(s, 0, sizeof(*s));
    assert_gethostbyname(&s->peer, host);
    s->peer.sin_port = htons(port);
    s->peer.sin_family = AF_INET;


    ASSERT_ERRNO(s->fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP));
    /* We are a client to kernel.c server, so do not need to bind.  An
       ephemeral port will get created, and reads on s->fd return
       packets returned to that port. */
    /* See packet_bridge.c for example UDP code. */

}


int main(int argc, char **argv) {
    if (argc != 3) {
        ERROR("usage: %s <host> <port>\n", argv[0]);
    }
    const char *host = argv[1];
    uint16_t port = atoi(argv[2]);

    struct tether_udp _s, *s = &_s;
    tether_udp_init(s, host, port);
    const char hello[] = "words\n";
    tether_udp_send_str(s, hello);
    return 0;
}
