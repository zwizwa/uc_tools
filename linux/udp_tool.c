/* The point of this thing is to provide an upload and command tool
   for i686/kernel.c based on idempotent commands.  For memory write,
   kernel just responds to packets, sends one ack per packet.  We keep
   going until we got an ack per block back.  This avoids tftp-style
   round-trip delays and keeps the implementation on the target very
   simple. */

/* See packet_bridge.c for example code. */

#include "macros.h"

#include "tcp_tools.h"


struct udp_tool {
    struct sockaddr_in peer;
    int fd;
};


void udp_tool_send(struct udp_tool *s, const uint8_t *buf, uint32_t len) {
    //log_addr(&p->peer);
    int flags = 0;
    int wlen;
    ASSERT_ERRNO(
        wlen = sendto(s->fd, buf, len, flags,
                      (struct sockaddr*)&s->peer,
                      sizeof(s->peer)));
    (void)wlen;
}
void udp_tool_send_str(struct udp_tool *s, const char *str) {
    udp_tool_send(s, (const uint8_t*)str, strlen(str));
}

void udp_tool_init(struct udp_tool *s, const char *host, uint16_t port) {
    memset(s, 0, sizeof(*s));
    assert_gethostbyname(&s->peer, host);
    s->peer.sin_port = htons(port);
    s->peer.sin_family = AF_INET;

    ASSERT_ERRNO(s->fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP));
    /* We are a client to kernel.c server, so do not need to bind.  An
       ephemeral port will get created, and reads on s->fd return
       packets returned to that port. */

}


int main(int argc, char **argv) {
    struct udp_tool _s, *s = &_s;
    udp_tool_init(s, "10.1.3.222", 1234);
    const char hello[] = "words\n";
    udp_tool_send_str(s, hello);
    return 0;
}
