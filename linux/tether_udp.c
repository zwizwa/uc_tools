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

/* Try to get everything integrated here. */
#define TETHER_3IF_UDP // Not enabled by default
#include "mod_tether_3if.c"

void tether_handle_async(struct tether *s) {
    ERROR("tether_udp: async not implemented\n");
}

#include <poll.h>

struct tether_udp {
    struct sockaddr_in peer;
    int fd;
    uint8_t sequence_number;
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

void usage_exit(int argc, char **argv) {
    LOG("usage: %s with arguments:\n");
    LOG("  console <host> <string>\n", argv[0]);
    LOG("  send    <host> <file>\n",   argv[0]);
    exit(1);
}

void cmd_console(int argc, char **argv) {
    if (argc != 3) { usage_exit(argc, argv); }
    const char *host    = argv[1];
    const char *command = argv[2];
    char command_n[strlen(command)+2];
    sprintf(command_n, "%s\n", command);
    struct tether_udp _s, *s = &_s;
    tether_udp_init(s, host, 1001);
    tether_udp_send_str(s, command_n);
    exit(0);
}

struct mon_packet {
    uint8_t  sequence_number;
    uint8_t  data[1471];
    uint16_t data_len;
};

void tether_udp_transact(struct tether_udp *s,
                         struct mon_packet *c) {
    c->sequence_number = s->sequence_number++;
    tether_udp_send(s, (void*)c, c->data_len);

    struct sockaddr_in from;
    socklen_t fromlen = sizeof(from);
    int flags = 0;

    struct pollfd pfd[] = {
        [0] = { .events = POLLIN, .fd = s->fd },
    };
    int rv;
    ASSERT_ERRNO(rv = poll(&pfd[0], ARRAY_SIZE(pfd), -1));
    ASSERT(rv >= 0);
    if (rv == 0) {
        LOG("timeout\n");
        exit(1);
    }
    ASSERT(pfd[0].revents & POLLIN);
    struct mon_packet r = {};

    rv = recvfrom(s->fd, &r, 1+sizeof(r.data), flags,
                  (struct sockaddr *)&from, &fromlen);
    // Note that 0 length is legal UDP but not part of 3if
    // protocol.  There will always be one sequence byte.
    ASSERT(rv >= 1);
    LOG("len %d seq=0x%02d\n", rv, r.sequence_number);

}


void cmd_send(int argc, char **argv) {
#if 0
    if (argc < 3) { usage_exit(argc, argv); }
    const char *host    = argv[2];
    const char *file    = argv[3];
    struct tether_udp _s, *s = &_s;
    tether_udp_init(s, host, 799);  // 0x31f
    struct mon_packet mp = {};
    mp.data_len = strlen(file); // FIXME
    memcpy(mp.data, file, mp.data_len);
    tether_udp_transact(s, &mp);
    tether_udp_transact(s, &mp);
    exit(0);
#else
    if (argc < 3) { usage_exit(argc, argv); }
    const char *host    = argv[2];
    const char *file    = argv[3];
    struct tether s;
    tether_open_udp(&s, host, 799); // 0x31f
    s.verbose  = 1;
    s.progress = 1;
    tether_dump_ram(&s, file,
                    0x7C00,
                    0x30000);
    exit(0);
#endif
}


int main(int argc, char **argv) {
    if (argc < 2)                         usage_exit(argc, argv);
    else if (!strcmp(argv[1], "console")) cmd_console(argc, argv);
    else if (!strcmp(argv[1], "send"))    cmd_send(argc, argv);
    else                                  usage_exit(argc, argv);
}

